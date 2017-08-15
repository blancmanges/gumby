{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (
    main
) where



-- gumby
import           Network.Slack.RTM
import           Network.Slack.WebAPI

-- base
import           Prelude hiding (id)
import           Control.Monad
import           System.Environment (lookupEnv)
import           System.Exit (exitWith, ExitCode(..))

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

-- wuss
import qualified Wuss as WSc

-- websockets
import qualified Network.WebSockets as WS



gumby :: WConnect -> Wr.Options -> WS.Connection -> IO void
gumby resp reqOpts conn = forever $ do
    msgFromSlack <- WS.receiveData conn
    putStrLn $ "got message: " ++ (msgFromSlack ^. LsLazy.utf8 . LsLazy.unpacked)

    case Ae.decode msgFromSlack of
        Nothing -> putStrLn "  message not recognized"
        Just x -> actions x

  where
    actions :: Event -> IO ()
    actions x = case x of
        Hello x          -> putStrLn $ "  events/Hello: " ++ show x
        MessageSent x    -> putStrLn $ "  our message sent: " ++ show x
        PresenceChange x -> putStrLn $ "  events/presence_change: " ++ show x
        ReconnectUrl x   -> putStrLn $ "  events/reconnect_url: " ++ show x
        UserTyping x     -> putStrLn $ "  events/user_typing: " ++ show x
        UnknownEvent     -> putStrLn $ "  ???"
        Message x | isGumbyCalledOut (x ^. text) -> do
                putStrLn $ "  events/message (for Gumby): " ++ show x
                addressedUser <- addressUserByName (x ^. user)
                let response = T.concat [ "Jo " , addressedUser , ", wassup?" ]
                rtmAPICall conn $ 
                    RSendMessage
                        { _rSendMessageId = 42
                        , _rSendMessageChannel = x ^. channel
                        , _rSendMessageText = response
                        }
        Message x | otherwise -> putStrLn $ "  events/message (not for Gumby): " ++ show x

    addressUserByName :: T.Text -> IO T.Text
    addressUserByName userId = do
        putStrLn "  ~~> asking about user details"
        userInfoResp <- webAPIRequest userId reqOpts
        case userInfoResp of
            Left err -> do
                putStrLn $ "  <~~ user query error: " ++ err
                pure "somebody"
            Right (userInfo :: WUsersInfo) -> do
                putStrLn $ "  <~~ resp: " ++ show userInfo
                putStrLn $ "    userName: " ++ (userInfo ^. name . LsStrict.unpacked)
                pure $ T.concat
                    [ "@"
                    , userInfo ^. name
                    ]

    isGumbyCalledOut :: T.Text -> Bool
    isGumbyCalledOut = T.isPrefixOf $ T.concat [ "<@" , _wConnectSelfId resp , ">" ]


main :: IO ()
main = do
    Just slackSecretApiToken <- (fmap T.pack) <$> lookupEnv "SLACK_SECRET_API_TOKEN"
    let reqOpts = Wr.defaults & param "token" .~ [slackSecretApiToken]

    rtmConn <- webAPIRequest () reqOpts

    case rtmConn of
        Left err -> do
            putStrLn $ "conn error: " ++ err
            exitWith $ ExitFailure 1
        Right (resp :: WConnect) ->
            WSc.runSecureClient
                (resp ^. urlHost . LsStrict.unpacked)
                rtmHostPort
                (resp ^. urlPath . LsStrict.unpacked)
                (gumby resp reqOpts)
  where
    rtmUrl = "https://slack.com/api/rtm.connect"
    rtmHostPort = 443
