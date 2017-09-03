{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Slack.API.RTM.Lens where


--
import           Network.Slack.API.RTM.Events.Message
import           Network.Slack.API.RTM.Request

-- base
import           Prelude hiding (id)

-- lens
import           Control.Lens



makeFields ''EMessage
makeFields ''RSendMessage
