{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Main (
    main
) where

-- (lib)
import qualified Network.Slack.API.Web.Methods.RtmConnect as Rtm
import qualified Network.Slack.API.RTM.Event              as Rtm
import qualified Network.Slack.API.RTM.Events.Message     as Rtm
import qualified Network.Slack.API.RTM.Request            as Rtm
import qualified Network.Slack.API.RTM.Lens               as Rtm
import qualified Network.Slack.API.Web.Lens    as Web
import qualified Network.Slack.API.Web.Classes as Web
-- (app)
import           Errors
import           App.Conf

-- base
import           Control.Monad
import           Data.Monoid
import           Data.Void
import           Control.Monad.IO.Class
import           System.Environment (lookupEnv)
import           Control.Concurrent
import           Control.Exception
import           System.IO (hPutStrLn, stderr)
import           Data.Function

-- mtl
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
-- (app)
import           Control.Monad.Reader.Extended  -- runReaderTWith

-- transformers
-- (app)
import           Control.Monad.Trans.Maybe.Extended -- liftMaybe

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- bytestring
import qualified Data.ByteString.Lazy as BSL

-- wreq
import qualified Network.Wreq as Wr
import           Network.Wreq (param)

-- lens
import           Control.Lens hiding ((.=))
import qualified Data.Text.Strict.Lens as LsStrict
import qualified Data.ByteString.Strict.Lens as BSS

-- aeson
import qualified Data.Aeson as Ae

-- wuss
import qualified Wuss as WSc

-- websockets
import qualified Network.WebSockets as WS

-- monad-logger
import           Control.Monad.Logger (
    MonadLogger, LoggingT
  , runStderrLoggingT, runChanLoggingT, unChanLoggingT, logDebug, logInfo, logError
  )

-- text-show
import           TextShow (showt)

-- monad-control
-- (app)
import           Control.Monad.Trans.Control.Extended (MonadBaseControl, control, controlLift2)

-- time
import qualified Data.Time as Time

-- tzdata
import qualified Data.Time.Zones.DB as TimeZones

-- tz
import qualified Data.Time.Zones as TimeZones
import qualified Data.Time.Zones.All as TimeZones

-- async
import           Control.Concurrent.Async (race_)



type IsBotMonad m = (MonadIO m, MonadReader BotState m)

type AppMonad = ReaderT AppState (ExceptT GumbyAppErr (LoggingT IO))
-- type RtmAppMonad = ReaderT RtmAppState (ExceptT GumbyAppErr (LoggingT IO))
-- type BotMonad = ReaderT BotState (ExceptT GumbyAppErr (LoggingT IO))

getRtmEvent :: IsBotMonad m => m (Either String Rtm.Event)
getRtmEvent = join $ view botRtmEventGetter

sendRtmRequest :: IsBotMonad m => Rtm.Request -> m ()
sendRtmRequest rq = do
    sender <- view botRtmRequestSender
    sender rq

runSimpleCommand :: forall m. IsBotMonad m => Text -> (Rtm.EMessage -> Text -> m ()) -> m ()
runSimpleCommand cmdPrefix callback = do
    res <- runMaybeT $ do
        rtmInput <- getRtmEvent
        msg <- liftMaybe (rtmInput ^? _Right . Web._Message)
        cmd <- liftMaybe . (T.strip <$>) . T.stripPrefix cmdPrefix . (^. Rtm.text) $ msg
        pure (msg, cmd)
    case res of
        Nothing -> pure ()
        Just (msg, cmd) -> callback msg cmd


main :: IO ()
main = bootstrapStage1
  where

    -- |Bootstrap: initialize logging and bottom part of monadic stack.
    bootstrapStage1 :: IO ()
    bootstrapStage1 = do
        chanLogs <- newChan
        race_
            (runLoggedApp chanLogs `finally` loggedAppFinished)
            (runLogger chanLogs `finally` loggerFinished)
      where
        runLoggedApp chanLogs = runChanLoggingT chanLogs $
            runExceptT (bootstrapStage2 chanLogs) >>= handleAppResult
        loggedAppFinished = hPutStrLn stderr "CORE ERROR - THREAD KILLED or FINISHED: logged app"

        handleAppResult (Right _) = $(logInfo)  "Program completed successfully"
        handleAppResult (Left  x) = $(logError) ("Program rejected: " <> showt x)

        runLogger = runStderrLoggingT . unChanLoggingT
        loggerFinished = hPutStrLn stderr "CORE ERROR - THREAD KILLED or FINISHED: logger"

    -- |Bootstrap: read config.
    bootstrapStage2 :: Chan LoggerData -> ExceptT GumbyAppErr (LoggingT IO) void
    bootstrapStage2 _cLoggingChan = do
        _cSecretSlackApiToken <- do
            query <- liftIO $ lookupEnv "SLACK_SECRET_API_TOKEN"
            case query of
                Nothing    -> throwError $ EnvVarMissing "SLACK_SECRET_API_TOKEN"
                Just value -> do
                    $(logDebug) "Reading the SLACK_SECRET_API_TOKEN from env vars"
                    pure . T.pack $ value

        _cWebsocketPingTime <- getEnvVarWithDefaults "WEBSOCKET_PING_TIME"
        _cRtmHostPort <- getEnvVarWithDefaults "WEBSOCKET_PORT"
        _cWebEndpoint <- getEnvVarWithDefaults "WEB_ENDPOINT"

        runReaderT bootstrapStage3 Config{..}

    bootstrapStage3 :: ReaderT (Config 'DWrapped) (ExceptT GumbyAppErr (LoggingT IO)) void
    bootstrapStage3 = do
        let configToAppState cfg = AppState{ _asConfig = unwrapCfg cfg }
        withReaderT configToAppState app

-- Thread structure:
--
--        +--- logging to stderr
--       /
--     -+--- core exception handler
--       \
--        +---app---+--- RTM listener
--                   \
--                    +--- Web sender
--                    +--- RTM sender
--                    +--- the bot
--
app :: AppMonad void
app = rtmBootstrapStage1
  where
    rtmBootstrapStage1 :: AppMonad void
    rtmBootstrapStage1 = do
        (rtmApiResponse :: Rtm.RtmConnectResp) <- do
            $(logDebug) "Getting websocket URL"
            token <- view (config . cSecretSlackApiToken)
            endpointRoot <- view cWebEndpoint
            response <- liftIO $ Wr.getWith
                            (Wr.defaults & param "token" .~ [token])
                            (endpointRoot <> "rtm.connect")
            let responseBody = response ^. Wr.responseBody
            case Ae.eitherDecode responseBody of
                Left  e -> do
                    $(logError) "Couldn't decode responseBody"
                    throwError $ JsonDecodeError e responseBody
                Right x -> pure x

        let wsUrlHost = rtmApiResponse ^. Web.urlHost . LsStrict.unpacked
            wsUrlPath = rtmApiResponse ^. Web.urlPath . LsStrict.unpacked
        rtmHostPort <- view (cRtmHostPort . to fromInteger)

        -- TODO: handle async exception from websockets (socket closed)
        $(logDebug) "Starting websocket process"
        x <- control $ \runInBase ->
            onException
                (WSc.runSecureClient
                    wsUrlHost rtmHostPort wsUrlPath
                    (runInBase . rtmBootstrapStage2 rtmApiResponse))
                (runInBase notifyExceptionRaised)

        $(logError) "WSc.runSecureClient ended, this should not happen"
        pure x
      where
        notifyExceptionRaised = $(logError) "WSc.runSecureClient killed by exception"

    rtmBootstrapStage2 :: Rtm.RtmConnectResp -> WS.Connection -> AppMonad void
    rtmBootstrapStage2 rtmApiResponse _rasConn = do
        _rasConfig <- view config

        $(logDebug) "Starting websocket pinger thread"
        pingTime <- view cWebsocketPingTime
        liftIO $ WS.forkPingThread _rasConn pingTime

        $(logDebug) "Creating inter-app communication chans"
        _rasChFromRtm  <- liftIO newChan
        _rasChToRtm    <- liftIO newChan
        _rasChToWeb    <- liftIO newChan
        _rasChFromBots <- liftIO newChan
        _rasChToGumby  <- liftIO newChan

        let _rasBotId = rtmApiResponse ^. Web.selfId
            appStateToRtmAppState _rasAppState = RtmAppState{..}
        withReaderT appStateToRtmAppState rtmBootstrapStage3

    rtmBootstrapStage3 :: ReaderT RtmAppState (ExceptT GumbyAppErr (LoggingT IO)) void
    rtmBootstrapStage3 = do
        -- TODO: pass readerT env to forked threads
        logCh <- view cLoggingChan
        chFromRtm <- view rasChFromRtm
        chToGumby <- view rasChToGumby
        botId <- view rasBotId
        rtmState <- view rtmAppState
        chanLogs <- view cLoggingChan
        chToRtm <- view rasChToRtm
        chToWeb <- view rasChToWeb
        conn <- view rasConn
        token <- view cSecretSlackApiToken
        endpointRoot <- view cWebEndpoint

        $(logDebug) "Spawning thread: copy messages from RTM to all bots"
        _ <- liftIO . forkIO . runChanLoggingT logCh $
            thToBots chFromRtm [chToGumby]

        $(logDebug) "Spawning thread: gumby"
        let notifyThreadFinished = $(logError) "Finished thread: gumby"
        _ <- liftIO . forkIO
                    . runChanLoggingT chanLogs
                    . controlLift2 (flip finally) notifyThreadFinished $ do
            let _botReallyReallyNeedsRtmAppState = rtmState

                _botRtmEventGetter :: forall m. MonadIO m => m (Either String Rtm.Event)
                _botRtmEventGetter = Ae.eitherDecode <$> liftIO (readChan chToGumby)

                _botRtmRequestSender :: forall m. MonadIO m => Rtm.Request -> m ()
                _botRtmRequestSender = liftIO . writeChan chToRtm . Ae.encode
                _botWebMethodCaller :: forall m req resp. (MonadIO m, Web.Method req resp)
                  => req
                  -> m (Either String resp)
                _botWebMethodCaller req = do
                    let httpEndpoint = Web.endpoint req
                        reqOpts = Web.urlEncode req Wr.defaults
                    chWebResponse <- liftIO $ newChan
                    liftIO $ writeChan chToWeb (reqOpts, httpEndpoint, chWebResponse)
                    resp <- liftIO $ readChan chWebResponse
                    pure $ Ae.eitherDecode resp

                _botCallout = "<@" <> botId <> ">"

            res <- runExceptT . runReaderTWith BotState{..} . vacuous $ gumby

            handleRtmAppResult res

        $(logDebug) "Spawning thread: Web API sender"
        _ <- liftIO . forkIO . runChanLoggingT logCh $
            thToWeb token endpointRoot chToWeb

        $(logDebug) "Spawning thread: RTM API sender"
        _ <- liftIO . forkIO . runChanLoggingT logCh $
            thToRtm conn chToRtm

        -- slack comm: receivers
        vacuous $ thFromRtm conn chFromRtm
        -- TODO: write a loop that waits for EXIT message -- https://stackoverflow.com/a/45846292/547223

    handleRtmAppResult :: Either GumbyAppErr Void -> LoggingT IO ()
    handleRtmAppResult (Right v) = absurd v
    handleRtmAppResult (Left err) = $(logError) ("RTM app rejected: " <> showt err)



gumby :: forall m void. IsBotMonad m => m void
gumby = do
    (callout :: Text) <- view botCallout
    forever $ runSimpleCommand callout go
  where
    genericErrReply = "(intensively thinking in confusion)…        Hello!"

    go :: Rtm.EMessage -> Text -> m ()
    go msg (T.toLower -> help)
      | help == "help" || help == "list"
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = "Commands available:\n    `tz` - show current time in various time zones"
          }

    go msg (T.toLower -> hi)
      | hi == "hello" || hi == "hi" || hi == "ohai"
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = "Hello!"
          }

    go msg "tz"
      = do
          currentUtc <- liftIO $ Time.getCurrentTime

          let tzs = [ TimeZones.America__Los_Angeles
                    , TimeZones.America__Chicago
                    , TimeZones.America__New_York
                    , TimeZones.Etc__UCT
                    , TimeZones.Europe__Warsaw
                    , TimeZones.Europe__Saratov
                    ]

              asdf :: TimeZones.TZLabel -> (TimeZones.TZ, TimeZones.TZLabel)
              asdf tz = (TimeZones.tzByLabel tz, tz)

              tzForUTC :: (TimeZones.TZ, TimeZones.TZLabel) -> (Time.TimeZone, TimeZones.TZLabel)
              tzForUTC (tz, tzl) = (TimeZones.timeZoneForUTCTime tz currentUtc, tzl)

              zonedTime :: (Time.TimeZone, TimeZones.TZLabel) -> (Time.ZonedTime, TimeZones.TZLabel)
              zonedTime (zone, tzl) = (Time.utcToZonedTime zone currentUtc, tzl)

              formatTime :: (Time.ZonedTime, TimeZones.TZLabel) -> (String, String)
              formatTime (zoned, tzl) = (tzName, zonedFmt)
                where
                    tzName = tzl ^. to TimeZones.toTZName . BSS.unpackedChars
                    zonedFmt = Time.formatTime Time.defaultTimeLocale "   --   %a, %Y-%m-%d at %H:%M   --   %Z (%z)" zoned

              align :: [(String, String)] -> [(String, String, String)]
              align xs = fmap alignRow xs
                where
                    maxLocationLength = maximum . fmap (\(x,_) -> length x) $ xs
                    alignRow (x, y) = (x, replicate (maxLocationLength - length x) ' ', y)

              buildSlackMsg :: [(String, String, String)] -> T.Text
              buildSlackMsg = T.pack . (\body -> "```" ++ body ++ "```") . unlines . fmap (\(x,y,z) -> x ++ y ++ z)

              reply = buildSlackMsg
                    . align
                    . fmap (formatTime . zonedTime . tzForUTC . asdf)
                    $ tzs

          sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
            { Rtm._rSendMessageId = 999
            , Rtm._rSendMessageChannel = msg ^. Rtm.channel
            , Rtm._rSendMessageText = reply
            }


    go msg _
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = genericErrReply
          }



-- TODO: bounded chans

-- TODO: drop, use dupChan instead
thToBots
  :: forall m void. (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => Chan BSL.ByteString -> [Chan BSL.ByteString] -> m void
thToBots incomingChan outgoingChans =
    control $ \runInBase ->
        finally (forever go) (runInBase notifyThreadFinished)
  where
    go :: IO ()
    go = do
        msg <- liftIO $ readChan incomingChan
        forM_ outgoingChans $ \outgoingChan ->
            liftIO $ writeChan outgoingChan msg
    notifyThreadFinished :: m ()
    notifyThreadFinished = $(logError) "Finished thread: thToBots"



thFromRtm :: forall m void. (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => WS.Connection -> Chan BSL.ByteString -> m void
thFromRtm conn chFromRtm =
    control $ \runInBase ->
        finally (forever go) (runInBase notifyThreadFinished)
  where
    go = do
        msgFromSlack <- liftIO $ WS.receiveData conn
        liftIO $ writeChan chFromRtm msgFromSlack
    notifyThreadFinished :: m ()
    notifyThreadFinished = $(logError) "Finished thread: thFromRtm"



thToRtm
  :: forall m void. (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => WS.Connection -> Chan BSL.ByteString -> m void
thToRtm conn chToRtm =
    control $ \runInBase ->
        finally (forever go) (runInBase notifyThreadFinished)
  where
    go :: IO ()
    go = do
        msgToSlack <- readChan chToRtm
        WS.sendTextData conn msgToSlack
    notifyThreadFinished :: m ()
    notifyThreadFinished = $(logError) "Finished thread: thToRtm"



thToWeb
  :: forall m void. (MonadLogger m, MonadIO m, MonadBaseControl IO m)
  => T.Text -> String -> Chan (Wr.Options, String, Chan BSL.ByteString) -> m void
thToWeb slackSecretApiToken endpointRoot chToWeb =
    control $ \runInBase ->
        finally (forever go) (runInBase notifyThreadFinished)
  where
    go :: IO ()
    go = do
        (reqOptions, endpoint, chReply) <- readChan chToWeb
        reply <- Wr.getWith
            (reqOptions & param "token" .~ [slackSecretApiToken])
            (endpointRoot <> endpoint)
        writeChan chReply (reply ^. Wr.responseBody)
    notifyThreadFinished :: m ()
    notifyThreadFinished = $(logError) "Finished thread: thToWeb"
