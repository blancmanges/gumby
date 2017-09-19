{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
    main
) where

-- (gumby)
import qualified Network.Slack.API.Web.Methods.RtmConnect as Rtm
import qualified Network.Slack.API.RTM.Event              as Rtm
import qualified Network.Slack.API.RTM.Request            as Rtm
import qualified Network.Slack.API.RTM.Lens               as Rtm
import qualified Network.Slack.API.Web.Lens     as Web
import qualified Network.Slack.API.Web.Classes  as Web
import           Network.Slack.API.Web.Methods.UsersList
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

-- containers
import qualified Data.Set as Set

-- bytestring
import qualified Data.ByteString.Lazy as BSL

-- wreq
import qualified Network.Wreq as Wr
import           Network.Wreq (param)

-- lens
import           Control.Lens hiding ((.=))
import qualified Data.Text.Strict.Lens as LsStrict

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
            WSc.runSecureClient wsUrlHost rtmHostPort wsUrlPath
                (runInIO . initiateBots slackSecretApiToken)


gumby
  :: MonadIO m
  => m (Either String Rtm.Event)
  -> (Rtm.Request -> m ())
  -> (forall req resp. Web.Method req resp => req -> m (Either String resp))
  -> m void
gumby getRtmEvent sendRtmRequest callWebMethod = forever $ do
    rtmInput <- getRtmEvent
    case rtmInput of
        Left _error -> pure ()
        Right event -> go event
  where
    go (Rtm.Message msg)

      | msg ^. Rtm.text == "hi gumby"
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = "ohai thar"}

      | msg ^. Rtm.text == "show timezones"
      = do
          result <- callWebMethod (UsersListReq { _usersListReqPresence = False })
          liftIO . putStrLn . show $ result
          let ourReply = case result of
                Left _         -> "???"
                Right (UsersListResp userList) ->
                    let getTzs x = (x ^. Web.tzLabel, x ^. Web.tzOffset)
                        showTz (label, offset) = show offset ++ " -- " ++ show label
                    in unlines . map showTz
                               . Set.toList
                               . Set.fromList
                               . map getTzs $ userList

          -- putStrLn $ show result
          sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
            { Rtm._rSendMessageId = 456
            , Rtm._rSendMessageChannel = msg ^. Rtm.channel
            , Rtm._rSendMessageText = ourReply ^. LsStrict.packed
            }

      | otherwise
      = do
          liftIO . putStrLn $ "got message: " ++ msg ^. Rtm.text . LsStrict.unpacked
          return ()

    go _ = return ()



startBotWithChans
  :: MonadIO m
  => Chan BSL.ByteString
  -> Chan BSL.ByteString
  -> Chan (Wr.Options, String, Chan BSL.ByteString)
  -> m void
startBotWithChans chIncoming chToRtm chToWeb =
    vacuous $ gumby getRtmEvent sendRtmRequest callWebMethod
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



initiateBots :: (MonadIO m, MonadLogger m) => T.Text -> WS.Connection -> m void
initiateBots slackSecretApiToken conn = do
    $(logDebug) "Creating comm chans"

    chFromRtm <- liftIO newChan
    chToRtm <- liftIO newChan
    chToWeb <- liftIO newChan

    chFromBots <- liftIO newChan
    chToGumby <- liftIO newChan

    $(logDebug) "Spawning thread: copy messages from RTM to all bots"
    liftIO . forkIO $ thToBots chFromRtm [chToGumby]

    $(logDebug) "Spawning thread: actual bot"
    liftIO . forkIO $ startBotWithChans chToGumby chToRtm chToWeb

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
