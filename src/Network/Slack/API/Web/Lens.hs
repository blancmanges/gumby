{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Slack.API.Web.Lens where

--
import           Network.Slack.API.Web.Methods
import           Network.Slack.API.Web.Objects

-- base
import           Prelude hiding (id)

-- lens
import           Control.Lens



makeFields ''RtmConnectResp
makeFields ''UsersInfoReq
makeFields ''UsersListReq
makeFields ''UsersListResp
makeFields ''User
