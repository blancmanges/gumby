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
  , runStderrLoggingT, logDebug, logInfo, logError
  )

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



main :: IO ()
main = runStderrLoggingT (handleErrors =<< runExceptT go)
  where
    rtmHostPort = 443

    handleErrors (Right ()) = $(logInfo)  "Program completed successfully"
    handleErrors (Left   x) = $(logError) ("Program rejected: " <> showt x)

    go :: ExceptT GumbyAppErr (LoggingT IO) ()
    go = do

        (slackSecretApiToken :: Text) <- do
            $(logDebug) "Reading the SLACK_SECRET_API_TOKEN from env vars"
            query <- liftIO $ lookupEnv "SLACK_SECRET_API_TOKEN"
            case query of
                Nothing    -> throwError $ EnvVarMissing "SLACK_SECRET_API_TOKEN"
                Just value -> pure . T.pack $ value

        (rtmApiResponse :: Rtm.RtmConnectResp) <- do
            $(logDebug) "Bootstrapping RTM connection to Slack"
            response <- liftIO $ Wr.getWith
                (Wr.defaults & param "token" .~ [slackSecretApiToken])
                (webAPIEndpointUrl "rtm.connect")
            let responseJSON = response ^. Wr.responseBody
                decodedJSON = Ae.eitherDecode responseJSON
            case decodedJSON of
                Left e  -> throwError $ JsonDecodeError e responseJSON
                Right x -> pure x

        let wsUrlHost = rtmApiResponse ^. Web.urlHost . LsStrict.unpacked
            wsUrlPath = rtmApiResponse ^. Web.urlPath . LsStrict.unpacked

        $(logDebug) "Starting websocket process"
        control $ \runInIO ->
            -- TODO: handle async exception from websockets (socket closed)
            WSc.runSecureClient wsUrlHost rtmHostPort wsUrlPath
                (runInIO . initiateBots slackSecretApiToken (rtmApiResponse ^. Web.selfId))


gumby
  :: forall m void. MonadIO m
  => T.Text
  -> m (Either String Rtm.Event)
  -> (Rtm.Request -> m ())
  -> (forall req resp. Web.Method req resp => req -> m (Either String resp))
  -> m void
gumby botName getRtmEvent sendRtmRequest _callWebMethod = forever $ do
    rtmInput <- getRtmEvent
    case rtmInput of
        Right (Rtm.Message msg) -> do
            let mMessage = msg ^. Rtm.text . to ((T.strip <$>) . T.stripPrefix slackEncodedCalloutPrefix)
            maybe
                (pure ())
                (go msg)
                mMessage
        _ -> pure ()
  where
    slackEncodedCalloutPrefix = "<@" <> botName <> ">"

    genericErrReply = "(intensively thinking in confusion)…        Hello!"

    go :: Rtm.EMessage -> Text -> m ()
    go msg (T.strip . T.toLower -> hi)
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



startBotWithChans
  :: forall m void. MonadIO m
  => T.Text
  -> Chan BSL.ByteString
  -> Chan BSL.ByteString
  -> Chan (Wr.Options, String, Chan BSL.ByteString)
  -> m void
startBotWithChans botName chIncoming chToRtm chToWeb =
    vacuous $ gumby botName getRtmEvent sendRtmRequest callWebMethod
  where
    getRtmEvent = Ae.eitherDecode <$> liftIO (readChan chIncoming)
    sendRtmRequest = liftIO . writeChan chToRtm . Ae.encode

    callWebMethod :: (Web.Method req resp, MonadIO m) => req -> m (Either String resp)
    callWebMethod req = do
        chWebResponse <- liftIO $ newChan
        liftIO $ writeChan chToWeb (reqOpts, httpEndpoint, chWebResponse)
        resp <- liftIO $ readChan chWebResponse
        pure $ Ae.eitherDecode resp
      where
        httpEndpoint = Web.endpoint req
        reqOpts = Web.urlEncode req Wr.defaults



initiateBots :: (MonadIO m, MonadLogger m) => T.Text -> T.Text -> WS.Connection -> m void
initiateBots slackSecretApiToken botName conn = do
    -- TODO: initialize comm chans *before* websocket conn is created in hope of recreating websockets
    $(logDebug) "Creating comm chans"

    chFromRtm <- liftIO newChan
    chToRtm <- liftIO newChan
    chToWeb <- liftIO newChan

    _chFromBots <- liftIO newChan
    chToGumby <- liftIO newChan

    $(logDebug) "Spawning thread: copy messages from RTM to all bots"
    liftIO . forkIO $ thToBots chFromRtm [chToGumby]

    $(logDebug) "Spawning thread: actual bot"
    liftIO . forkIO $ startBotWithChans botName chToGumby chToRtm chToWeb

    -- slack comm: senders
    $(logDebug) "Spawnig thread: Web API sender"
    liftIO . forkIO $ thToWeb slackSecretApiToken chToWeb
    $(logDebug) "Spawnig thread: RTM API sender"
    liftIO . forkIO $ thToRtm conn chToRtm

    -- slack comm: receivers
    vacuous $ thFromRtm conn chFromRtm
    -- TODO: write a loop that waits for EXIT message



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
    writeChan chReply $ reply ^. Wr.responseBody



webAPIEndpointUrl :: String -> String
webAPIEndpointUrl = ("https://slack.com/api/" ++)
