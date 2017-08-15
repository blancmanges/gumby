{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where



-- gumby
import           Network.Slack.Events

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



data RtmStartRes
    = RtmStartRes
        { _rtmStartUrl :: URI.URI
        , _rtmStartUrlHost :: String
        , _rtmStartUrlPath :: String
        , _rtmStartSelfId :: T.Text
        , _rtmStartSelfName :: T.Text
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
        self <- o .: "self"
        selfId <- self .: "id"
        selfName <- self .: "name"
        return RtmStartRes
            { _rtmStartUrl = urlParsed
            , _rtmStartUrlHost = urlHost
            , _rtmStartUrlPath = urlPath
            , _rtmStartSelfId = selfId
            , _rtmStartSelfName = selfName
            } 


data RtmSendMsg
    = RtmSendMsg
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



data UsersInfo
    = UsersInfo
        { _usersInfoId :: T.Text
        , _usersInfoName :: T.Text
        }
  deriving (Show)
makeLenses ''UsersInfo

instance Ae.FromJSON UsersInfo where
    parseJSON (Ae.Object o) = do
        ok <- o .: "ok"
        guard ok
        user <- o .: "user"
        UsersInfo
            <$> user .: "id"
            <*> user .: "name"


gumby :: RtmStartRes -> T.Text -> T.Text -> WS.Connection -> IO void
gumby resp chanId slackSecretApiToken conn = forever $ do
    msgFromSlack <- WS.receiveData conn
    putStrLn $ "got message: " ++ (msgFromSlack ^. LsLazy.utf8 . LsLazy.unpacked)

    case Ae.decode msgFromSlack of
        Nothing -> pure ()
        Just e  -> case e of
            Hello x          -> putStrLn $ "  events/Hello: " ++ show x
            MessageSent x    -> putStrLn $ "  our message sent: " ++ show x
            PresenceChange x -> putStrLn $ "  events/presence_change: " ++ show x
            ReconnectUrl x   -> putStrLn $ "  events/reconnect_url: " ++ show x
            UserTyping x     -> putStrLn $ "  events/user_typing: " ++ show x
            UnknownEvent     -> putStrLn $ "  ???"
            Message x        -> do
                putStrLn $ "  events/message: " ++ show x
                when (gumbyCallout `T.isPrefixOf` (x ^. text)) $ do
                    putStrLn "  ~~> asking about user details"
                    let opts =
                            Wr.defaults
                                & param "token" .~ [slackSecretApiToken]
                                & param "user" .~ [x ^. user]

                    respEncoded <- Wr.getWith opts "https://slack.com/api/users.info"
                    userName <-
                        case Ae.eitherDecode (respEncoded ^. Wr.responseBody) of
                            Left err -> do
                                putStrLn $ "  <~~ error: " ++ err
                                pure "man"
                            Right (ui :: UsersInfo) -> do
                                putStrLn $ "  <~~ resp: " ++ show ui
                                putStrLn $ "    userName: " ++ (ui ^. usersInfoName . LsStrict.unpacked)
                                pure $ ui ^. usersInfoName

                    let response = T.concat
                            [ "Jo @"
                            , userName
                            , ", wassup?"
                            ]
                    WS.sendTextData conn . Ae.encode $ 
                        RtmSendMsg
                            { _rtmSendId = 42
                            , _rtmSendChannel = x ^. channel
                            , _rtmSendText = response
                            }

  where
    gumbyUid = resp ^. rtmStartSelfId
    gumbyCallout = T.concat
        [ "<@" 
        , gumbyUid
        , ">"
        ]


main :: IO ()
main = do
    Just slackSecretApiToken <- (fmap T.pack) <$> lookupEnv "SLACK_SECRET_API_TOKEN"
    Just slackChanId <- (fmap T.pack) <$> lookupEnv "SLACK_CHAN_ID"

    let rtmStartOpts = Wr.defaults & param "token" .~ [slackSecretApiToken]
    respEncoded <- Wr.getWith rtmStartOpts rtmUrl

    putStrLn $ "conn response: " ++ (respEncoded ^. Wr.responseBody . LsLazy.utf8 . LsLazy.unpacked)

    let Right resp = Ae.eitherDecode (respEncoded ^. Wr.responseBody)

    WSc.runSecureClient
        (resp ^. rtmStartUrlHost)
        rtmHostPort
        (resp ^. rtmStartUrlPath)
        (gumby resp slackChanId slackSecretApiToken)

  where
    rtmUrl = "https://slack.com/api/rtm.connect"
    rtmHostPort = 443
