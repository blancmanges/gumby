{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.Web.Methods.RtmConnect where

--
import           Network.Slack.API.Web.Classes

-- base
import           Control.Monad

-- text
import           Data.Text (Text)

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson (FromJSON, (.:))

-- uri-bytestring
import           URI.ByteString
  ( URI, authorityL, authorityHostL, parseURI, strictURIParserOptions, hostBSL, pathL
  )

-- lens
import           Control.Lens
import qualified Data.Text.Strict.Lens as LsStrict



data RtmConnectReq
  = RtmConnectReq
  deriving (Show)

instance UrlEncode RtmConnectReq where urlEncode _ = id



data RtmConnectResp
  = RtmConnectResp
      { _rtmConnectRespUrl      :: URI
      , _rtmConnectRespUrlHost  :: Text
      , _rtmConnectRespUrlPath  :: Text
      , _rtmConnectRespSelfId   :: Text
      , _rtmConnectRespSelfName :: Text
      }
  deriving (Show)

instance FromJSON RtmConnectResp where
  parseJSON = Ae.withObject "RtmConnectResp" $ \o -> do
    guard =<< o .: "ok"

    url      <- o .: "url"
    self     <- o .: "self"
    selfId   <- self .: "id"
    selfName <- self .: "name"

    Just urlParsed  <- pure $ url       ^? re LsStrict.utf8 . to (parseURI strictURIParserOptions) . _Right
    Just  urlHost   <- pure $ urlParsed ^? authorityL . _Just . authorityHostL . hostBSL . LsStrict.utf8
    Just  urlPath   <- pure $ urlParsed ^? pathL . LsStrict.utf8

    pure RtmConnectResp
      { _rtmConnectRespUrl      = urlParsed
      , _rtmConnectRespUrlHost  = urlHost
      , _rtmConnectRespUrlPath  = urlPath
      , _rtmConnectRespSelfId   = selfId
      , _rtmConnectRespSelfName = selfName
      } 

instance Method RtmConnectReq RtmConnectResp where
  endpoint _ = "rtm.connect"
