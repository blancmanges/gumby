{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Slack.WebAPI where


-- base
import           Control.Monad

-- lens
import           Control.Lens hiding ((.=))
import qualified Data.Text.Lazy.Lens as LsLazy
import qualified Data.Text.Strict.Lens as LsStrict

-- text
import qualified Data.Text as T

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson ((.:), (.=))

-- wreq
import qualified Network.Wreq as Wr
import           Network.Wreq (param)

-- uri-bytestring
import qualified URI.ByteString as URI



webAPIEndpoint :: String -> String
webAPIEndpoint = ("https://slack.com/api/" ++)



class WebAPI req where
    type Arguments req
    webAPIRequest :: Arguments req -> Wr.Options -> IO (Either String req)



data WUsersInfo
  = WUsersInfo
      { _wUsersInfoId :: T.Text
      , _wUsersInfoName :: T.Text
      }
  deriving (Show)
makeFields ''WUsersInfo

instance Ae.FromJSON WUsersInfo where
    parseJSON (Ae.Object o) = do
        ok <- o .: "ok"
        guard ok
        user <- o .: "user"
        WUsersInfo
            <$> user .: "id"
            <*> user .: "name"

instance WebAPI WUsersInfo where
    type Arguments WUsersInfo = T.Text
    webAPIRequest userId reqOpts = do
        let opts = reqOpts & param "user" .~ [userId]
        response <- Wr.getWith opts $ webAPIEndpoint "users.info"
        pure . Ae.eitherDecode $ response ^. Wr.responseBody


data WConnect
  = WConnect
      { _wConnectUrl :: URI.URI
      , _wConnectUrlHost :: T.Text
      , _wConnectUrlPath :: T.Text
      , _wConnectSelfId :: T.Text
      , _wConnectSelfName :: T.Text
      }
  deriving (Show, Eq)
makeFields ''WConnect

instance Ae.FromJSON WConnect where
    parseJSON (Ae.Object o) = do
        ok <- o .: "ok"
        guard ok
        url <- o .: "url"
        -- TODO
        let Just urlParsed  = url       ^? re LsStrict.utf8 . to (URI.parseURI URI.strictURIParserOptions) . _Right
            Just  urlHost   = urlParsed ^? URI.authorityL . _Just . URI.authorityHostL . URI.hostBSL . LsStrict.utf8
            Just  urlPath   = urlParsed ^? URI.pathL . LsStrict.utf8
        self <- o .: "self"
        selfId <- self .: "id"
        selfName <- self .: "name"
        return WConnect
            { _wConnectUrl = urlParsed
            , _wConnectUrlHost = urlHost
            , _wConnectUrlPath = urlPath
            , _wConnectSelfId = selfId
            , _wConnectSelfName = selfName
            } 

instance WebAPI WConnect where
    type Arguments WConnect = ()
    webAPIRequest _ reqOpts = do
        response <- Wr.getWith reqOpts (webAPIEndpoint "rtm.connect")
        pure . Ae.eitherDecode $ response ^. Wr.responseBody
