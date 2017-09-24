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

module Main (
    main
) where

-- (gumby)
import qualified Network.Slack.API.Web.Methods.RtmConnect as Rtm
import qualified Network.Slack.API.RTM.Event              as Rtm
import qualified Network.Slack.API.RTM.Events.Message     as Rtm
import qualified Network.Slack.API.RTM.Request            as Rtm
import qualified Network.Slack.API.RTM.Lens               as Rtm
import qualified Network.Slack.API.Web.Lens    as Web
import qualified Network.Slack.API.Web.Classes as Web
-- (app)
import           Errors

-- base
import           Control.Monad
import           Data.Monoid
import           Data.Void
import           Control.Monad.IO.Class
import           System.Environment (lookupEnv)
import           Control.Concurrent

-- mtl
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader

-- transformers
import           Control.Monad.Trans.Maybe

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
import qualified Control.Monad.Logger as Logger

-- text-show
import           TextShow (showt)

-- monad control
import           Control.Monad.Trans.Control (control)

-- time
import qualified Data.Time as Time

-- tzdata
import qualified Data.Time.Zones.DB as TimeZones

-- tz
import qualified Data.Time.Zones as TimeZones
import qualified Data.Time.Zones.All as TimeZones



-- |The type used by monad-logger for inter-process logging.
type LoggerData = (Logger.Loc, Logger.LogSource, Logger.LogLevel, Logger.LogStr)

-- |Config of the app.
data Config
  = Config
      { _cSecretSlackApiToken :: Text
      , _cLoggingChan :: Chan LoggerData
      }

-- |App state. Includes config and app state.
data AppState
  = AppState
      { _asConfig      :: Config
      }

-- |RTM app state. Includes config, app state, data from Slack, and other runtime state
-- for the bot.
data RtmAppState
  = RtmAppState
      { _rasAppState   :: AppState
      , _rasBotId      :: Text
      , _rasConn       :: WS.Connection
      , _rasChFromRtm  :: Chan BSL.ByteString
      , _rasChToRtm    :: Chan BSL.ByteString
      , _rasChToWeb    :: Chan (Wr.Options, String, Chan BSL.ByteString)
      , _rasChFromBots :: Chan Void
      , _rasChToGumby  :: Chan BSL.ByteString
      }

data BotState
  = BotState
      { _botReallyReallyNeedsRtmAppState :: RtmAppState
      , _botCallout :: Text
      , _botRtmEventGetter :: forall m. MonadIO m => m (Either String Rtm.Event)
      , _botRtmRequestSender :: forall m. MonadIO m => Rtm.Request -> m ()
      , _botWebMethodCaller
          :: forall m req resp. (MonadIO m, Web.Method req resp)
          => req
          -> m (Either String resp)
      }

makeClassy ''Config
makeClassy ''AppState
makeClassy ''RtmAppState
makeClassy ''BotState

instance HasConfig AppState where
    config = asConfig

instance HasConfig RtmAppState where
    config = rasAppState . asConfig

instance HasAppState RtmAppState where
    appState = rasAppState

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
        msg <- continueWithValue (rtmInput ^? _Right . Web._Message)
        cmd <- continueWithValue . (T.strip <$>) . T.stripPrefix cmdPrefix . (^. Rtm.text) $ msg
        pure (msg, cmd)
    case res of
        Nothing -> pure ()
        Just (msg, cmd) -> callback msg cmd
  where
    continueWithValue :: Maybe a -> MaybeT m a
    continueWithValue = MaybeT . pure




main :: IO ()
main = bootstrapStage1
  where

    -- |Gather and process result of running of the application.
    -- Resource handling should happen here.
    handleAppResult (Right _) = $(logInfo)  "Program completed successfully"
    handleAppResult (Left  x) = $(logError) ("Program rejected: " <> showt x)

    -- |Bootstrap: initialize logging and bottom part of monadic stack.
    bootstrapStage1 :: IO void
    bootstrapStage1 = do
        chanLogs <- newChan
        _ <- forkIO . runChanLoggingT chanLogs $
            runExceptT (bootstrapStage2 chanLogs) >>= handleAppResult
        vacuous . runStderrLoggingT $ unChanLoggingT chanLogs

    -- |Bootstrap: read config.
    bootstrapStage2 :: Chan LoggerData -> ExceptT GumbyAppErr (LoggingT IO) WS.Connection
    bootstrapStage2 _cLoggingChan = do
        _cSecretSlackApiToken <- do
            $(logDebug) "Reading the SLACK_SECRET_API_TOKEN from env vars"
            query <- liftIO $ lookupEnv "SLACK_SECRET_API_TOKEN"
            case query of
                Nothing    -> throwError $ EnvVarMissing "SLACK_SECRET_API_TOKEN"
                Just value -> pure . T.pack $ value

        runReaderT bootstrapStage3 Config{..}

    bootstrapStage3 :: ReaderT Config (ExceptT GumbyAppErr (LoggingT IO)) WS.Connection
    bootstrapStage3 = do
        let configToAppState _asConfig = AppState{..}
        withReaderT configToAppState app

-- Thread structure:
--
--     -+--- logging to stderr
--       \
--        +---app---+--- RTM listener
--                   \
--                    +--- Web sender
--                    +--- RTM sender
--                    +--- the bot
--
app :: AppMonad WS.Connection
app = rtmBootstrapStage1
  where
    rtmHostPort = 443

    rtmBootstrapStage1 :: AppMonad WS.Connection
    rtmBootstrapStage1 = do
        (rtmApiResponse :: Rtm.RtmConnectResp) <- do
            $(logDebug) "Getting websocket URL"
            token <- view (config . cSecretSlackApiToken)
            response <- liftIO $ Wr.getWith
                            (Wr.defaults & param "token" .~ [token])
                            (webAPIEndpointUrl "rtm.connect")
            let responseBody = response ^. Wr.responseBody
            case Ae.eitherDecode responseBody of
                Left  e -> throwError $ JsonDecodeError e responseBody
                Right x -> pure x

        let wsUrlHost = rtmApiResponse ^. Web.urlHost . LsStrict.unpacked
            wsUrlPath = rtmApiResponse ^. Web.urlPath . LsStrict.unpacked

        -- TODO: handle async exception from websockets (socket closed)
        $(logDebug) "Starting websocket process"
        control $ \runInBase ->
            WSc.runSecureClient
                wsUrlHost rtmHostPort wsUrlPath
                (runInBase . rtmBootstrapStage2 rtmApiResponse)

    rtmBootstrapStage2 :: Rtm.RtmConnectResp -> WS.Connection -> AppMonad void
    rtmBootstrapStage2 rtmApiResponse _rasConn = do
        _rasConfig <- view config

        $(logDebug) "Creating inter-app communication chans"
        _rasChFromRtm  <- liftIO newChan
        _rasChToRtm    <- liftIO newChan
        _rasChToWeb    <- liftIO newChan
        _rasChFromBots <- liftIO newChan
        _rasChToGumby  <- liftIO newChan

        let _rasBotId = rtmApiResponse ^. Web.selfId
            appStateToRtmAppState _rasAppState = RtmAppState{..}
        withReaderT appStateToRtmAppState rtmBootstrapStage3

    -- TODO: async handlers
    rtmBootstrapStage3 :: ReaderT RtmAppState (ExceptT GumbyAppErr (LoggingT IO)) void
    rtmBootstrapStage3 = do
        chFromRtm <- view rasChFromRtm
        chToGumby <- view rasChToGumby
        botId <- view rasBotId
        rtmState <- view rtmAppState
        chanLogs <- view cLoggingChan
        chToRtm <- view rasChToRtm
        chToWeb <- view rasChToWeb
        conn <- view rasConn
        token <- view cSecretSlackApiToken

        $(logDebug) "Spawning thread: copy messages from RTM to all bots"
        _ <- liftIO . forkIO $ thToBots chFromRtm [chToGumby]

        $(logDebug) "Spawning thread: gumby"
        _ <- liftIO . forkIO . runChanLoggingT chanLogs $ do
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

            res <- runExceptT $ runReaderT (vacuous gumby) BotState{..}
            -- TODO: oh snap, get rid of RecordWildCards :< no error ^^^^ here?…

            handleRtmAppResult res

        -- slack comm: senders
        $(logDebug) "Spawnig thread: Web API sender"
        _ <- liftIO . forkIO $ thToWeb token chToWeb
        $(logDebug) "Spawnig thread: RTM API sender"
        _ <- liftIO . forkIO $ thToRtm conn chToRtm

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
thToBots :: (MonadIO m)
  => Chan BSL.ByteString -> [Chan BSL.ByteString] -> m void
thToBots incomingChan outgoingChans = forever $ do
    msg <- liftIO $ readChan incomingChan
    forM_ outgoingChans $ \outgoingChan ->
        liftIO $ writeChan outgoingChan msg



thFromRtm :: (MonadIO m, MonadLogger m)
  => WS.Connection -> Chan BSL.ByteString -> m void
thFromRtm conn chFromRtm = forever $ do
    msgFromSlack <- liftIO $ WS.receiveData conn
    -- $(logDebug) ("thFromRtm: got message: " <> showt msgFromSlack)
    liftIO $ writeChan chFromRtm msgFromSlack



thToRtm :: WS.Connection -> Chan BSL.ByteString -> IO void
thToRtm conn chToRtm = forever $ do
    msgToSlack <- readChan chToRtm
    WS.sendTextData conn msgToSlack



thToWeb :: T.Text -> Chan (Wr.Options, String, Chan BSL.ByteString) -> IO void
thToWeb slackSecretApiToken chToWeb = forever $ do
    (reqOptions, endpoint, chReply) <- readChan chToWeb
    reply <- Wr.getWith
        (reqOptions & param "token" .~ [slackSecretApiToken])
        (webAPIEndpointUrl endpoint)
    -- putStrLn $ "web:" ++ (reply ^. Wr.responseBody . BSL.unpackedChars)
    -- $(logDebug) ("thToWeb: got reply: " <> (reply ^. Wr.responseBody . to showt))
    writeChan chReply (reply ^. Wr.responseBody)



webAPIEndpointUrl :: String -> String
webAPIEndpointUrl = ("https://slack.com/api/" ++)


-- TODO: Dockerize
