{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
    main
) where



-- base
import           Control.Monad
import           System.Environment (lookupEnv)

-- text
import qualified Data.Text as T

-- wreq
import qualified Network.Wreq as Wr
import           Network.Wreq (param)

-- lens
import           Control.Lens hiding ((.=))
import qualified Data.Text.Lazy.Lens as LsLazy
import qualified Data.Text.Strict.Lens as LsStrict

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson ((.:), (.=))

-- uri-bytestring
import qualified URI.ByteString as URI

-- wuss
import qualified Wuss as WSc

-- websockets
import qualified Network.WebSockets as WS



data RtmStartRes =
    RtmStartRes
        { _rtmStartUrl :: URI.URI
        , _rtmStartUrlHost :: String
        , _rtmStartUrlPath :: String
        }
  deriving (Show)
makeLenses ''RtmStartRes

instance Ae.FromJSON RtmStartRes where
    parseJSON (Ae.Object o) = do
        ok <- o .: "ok"
        guard ok
        url <- o .: "url"
        let Just urlParsed  = url       ^? re LsStrict.utf8 . to (URI.parseURI URI.strictURIParserOptions) . _Right
            Just  urlHost   = urlParsed ^? URI.authorityL . _Just . URI.authorityHostL . URI.hostBSL . LsStrict.utf8 . LsStrict.unpacked
            Just  urlPath   = urlParsed ^? URI.pathL . LsStrict.utf8 . LsStrict.unpacked
        return RtmStartRes
            { _rtmStartUrl = urlParsed
            , _rtmStartUrlHost = urlHost
            , _rtmStartUrlPath = urlPath
            } 


data RtmSendMsg =
    RtmSendMsg
        { _rtmSendId :: Int
        , _rtmSendChannel :: T.Text
        , _rtmSendText :: T.Text
        }
    deriving (Show)
makeLenses ''RtmSendMsg

instance Ae.ToJSON RtmSendMsg where
    toJSON x = Ae.object [ "id"      .= (x ^. rtmSendId)
                         , "type"    .= ("message" :: String)
                         , "channel" .= (x ^. rtmSendChannel)
                         , "text"    .= (x ^. rtmSendText)
                         ]


gumby :: T.Text -> WS.Connection -> IO void
gumby chanId conn = forever $ do
    msgFromSlack <- WS.receiveData conn
    putStrLn $ "got some message: " ++ (msgFromSlack ^. LsStrict.utf8 . LsStrict.unpacked)
    let reply = RtmSendMsg
                    { _rtmSendId = 1
                    , _rtmSendChannel = chanId
                    , _rtmSendText = "ACKd!"
                    }
        replyE = Ae.encode reply
    putStrLn $ "replying with: " ++ (replyE ^. LsLazy.utf8 . LsLazy.unpacked)
    WS.sendTextData conn replyE
    

main :: IO ()
main = do
    Just slackSecretApiToken <- (fmap T.pack) <$> lookupEnv "SLACK_SECRET_API_TOKEN"
    Just slackChanId <- (fmap T.pack) <$> lookupEnv "SLACK_CHAN_ID"

    let rtmStartOpts = Wr.defaults & param "token" .~ [slackSecretApiToken]
    respEncoded <- Wr.getWith rtmStartOpts "https://slack.com/api/rtm.start"
    let Right resp = Ae.eitherDecode (respEncoded ^. Wr.responseBody)

    WSc.runSecureClient (resp ^. rtmStartUrlHost) 443 (resp ^. rtmStartUrlPath) (gumby slackChanId)

    putStrLn "DOCTOR"
    putStrLn "My brain hurts"
    putStrLn "GOODBYE"