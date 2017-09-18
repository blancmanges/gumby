{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.Web.Methods.UsersList where

--
import           Network.Slack.API.Web.Classes
import           Network.Slack.API.Web.Objects.User

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

-- http-api-data
import qualified Web.HttpApiData as WData



data UsersListReq
  = UsersListReq
      { _usersListReqPresence :: Bool }
  deriving (Show)

instance UrlEncode UsersListReq where
  urlEncode req opts =
      opts & param "presence" .~ [WData.toUrlPiece . _usersListReqPresence $ req]



newtype UsersListResp = UsersListResp { _usersListRespResp :: [User] }
  deriving (Show)

instance FromJSON UsersListResp where
  parseJSON = Ae.withObject "UsersListResp" $ \o -> do
    guard =<< o .: "ok"
    UsersListResp <$> o .: "members"

instance Method UsersListReq UsersListResp where
  endpoint _ = "users.list"
