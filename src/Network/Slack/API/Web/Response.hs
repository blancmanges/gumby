{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Network.Slack.API.Web.Response where

--
import           Network.Slack.API.Web.Methods



data Response
  = RtmConnect RtmConnectResp
  | UsersInfo  UsersInfoResp
  | UsersList  UsersListResp
  deriving (Show)
