{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main (
    main
) where

--
import qualified Network.Slack.API.Web.Methods.RtmConnect as Rtm
import qualified Network.Slack.API.RTM.Event              as Rtm
import qualified Network.Slack.API.RTM.Request            as Rtm
import qualified Network.Slack.API.RTM.Lens               as Rtm
import qualified Network.Slack.API.Web.Lens     as Web
import qualified Network.Slack.API.Web.Classes  as Web

-- base
import           Control.Monad
import           System.Environment (lookupEnv)
import           Control.Concurrent

-- text
import qualified Data.Text as T

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



webAPIEndpoint :: String -> String
webAPIEndpoint = ("https://slack.com/api/" ++)


main :: IO ()
main = do
    Just slackSecretApiToken <- (fmap T.pack) <$> lookupEnv "SLACK_SECRET_API_TOKEN"

    rtmResp <- do
        response <- Wr.getWith
            (Wr.defaults & param "token" .~ [slackSecretApiToken])
            (webAPIEndpoint "rtm.connect")
        let Right (x :: Rtm.RtmConnectResp) = Ae.eitherDecode $ response ^. Wr.responseBody
        pure x

    WSc.runSecureClient
        (rtmResp  ^. Web.urlHost . LsStrict.unpacked)
        rtmHostPort
        (rtmResp ^. Web.urlPath . LsStrict.unpacked)
        (startBots slackSecretApiToken)

  where
    rtmHostPort = 443



gumbyWrap ::
     Chan BSL.ByteString
  -> Chan BSL.ByteString
  -> Chan (Wr.Options, String, Chan BSL.ByteString)
  -> IO void
gumbyWrap chIncoming chToRtm chToWeb = gumby getRtmEvent sendRtmRequest callWebMethod
  where
    getRtmEvent = Ae.eitherDecode <$> readChan chIncoming
    sendRtmRequest = writeChan chToRtm . Ae.encode
    callWebMethod ::
         Web.Method req resp
      => req
      -> IO (Either String resp)
    callWebMethod req = do
        chWebResponse <- newChan
        writeChan chToWeb (reqOpts, httpEndpoint, chWebResponse)
        resp <- readChan chWebResponse
        pure $ Ae.eitherDecode resp
      where
        httpEndpoint = Web.endpoint req
        reqOpts = Web.urlEncode req Wr.defaults



gumby ::
     IO (Either String Rtm.Event)
  -> (Rtm.Request -> IO ())
  -> (forall req resp. Web.Method req resp => req -> IO (Either String resp))
  -> IO void
gumby getRtmEvent sendRtmRequest callWebMethod = forever $ do
    rtmInput <- getRtmEvent
    case rtmInput of
        Left err -> pure ()
        Right event -> go event
  where
    go :: Rtm.Event -> IO ()
    go (Rtm.Message msg)
      | msg ^. Rtm.text == "hi gumby"
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = "ohai thar"}
      | otherwise
      = do
        putStrLn $ "got message: " ++ msg ^. Rtm.text . LsStrict.unpacked
        return ()
    go _ = return ()



startBots :: T.Text -> WS.Connection -> IO void
startBots slackSecretApiToken conn = do
    chFromRtm <- newChan
    chToRtm <- newChan
    chToWeb <- newChan

    chFromBots <- newChan
    chToGumby <- newChan

    forkIO $ thToBots chFromRtm [chToGumby]

    forkIO $ gumbyWrap chToGumby chToRtm chToWeb

    -- slack comm: senders
    forkIO $ thToWeb slackSecretApiToken chToWeb
    forkIO $ thToRtm conn chToRtm

    -- slack comm: receivers
    thFromRtm conn chFromRtm
    -- TODO: write a loop that waits for EXIT message



-- TODO: drop, use dupChan instead
thToBots :: Chan BSL.ByteString -> [Chan BSL.ByteString] -> IO void
thToBots incomingChan outgoingChans = forever $ do
    msg <- readChan incomingChan
    forM_ outgoingChans $ \outgoingChan -> writeChan outgoingChan msg



thFromRtm :: WS.Connection -> Chan BSL.ByteString -> IO void
thFromRtm conn chFromRtm = forever $ do
    msgFromSlack <- WS.receiveData conn
    writeChan chFromRtm msgFromSlack



thToRtm :: WS.Connection -> Chan BSL.ByteString -> IO void
thToRtm conn chToRtm = forever $ do
    msgToSlack <- readChan chToRtm
    WS.sendTextData conn msgToSlack



thToWeb :: T.Text -> Chan (Wr.Options, String, Chan BSL.ByteString) -> IO void
thToWeb slackSecretApiToken chToWeb = forever $ do
    (reqOptions, endpoint, chReply) <- readChan chToWeb
    reply <- Wr.getWith
        (reqOptions & param "token" .~ [slackSecretApiToken])
        (webAPIEndpoint endpoint)
    writeChan chReply $ reply ^. Wr.responseBody
