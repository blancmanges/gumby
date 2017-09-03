{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.Web.Methods.UsersInfo where

--
import           Network.Slack.API.Web.Classes

-- base
import           Control.Monad

-- text
import           Data.Text (Text)

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson (FromJSON, (.:))

-- lens
import           Control.Lens

-- wreq
import           Network.Wreq (param)



data UsersInfoReq
  = UsersInfoReq
      { _usersInfoReqUserId :: Text }
  deriving (Show)

instance UrlEncode UsersInfoReq where
  urlEncode req opts = opts & param "user" .~ [_usersInfoReqUserId req]



data UsersInfoResp
  = UsersInfoResp
      { _usersInfoRespId :: Text
      , _usersInfoRespName :: Text
      }
  deriving (Show)

instance FromJSON UsersInfoResp where
  parseJSON = Ae.withObject "UsersInfoResp" $ \o -> do
    ok <- o .: "ok"
    guard ok

    user <- o .: "user"
    uId <- user .: "id"
    uName <- user .: "name"

    pure UsersInfoResp
      { _usersInfoRespId = uId
      , _usersInfoRespName = uName
      }

instance Method UsersInfoReq UsersInfoResp where
  endpoint _ = "users.info"
