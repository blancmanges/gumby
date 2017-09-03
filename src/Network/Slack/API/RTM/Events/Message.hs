{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Slack.API.RTM.Events.Message where


-- base
import           Data.List
import           Data.Maybe

-- text
import           Data.Text (Text)

-- aeson
import           Data.Aeson.Types (camelTo2)
import           Data.Aeson.TH (deriveFromJSON, defaultOptions, Options(..))



-- https://api.slack.com/events/message
data EMessage
  = EMessage
      { _eMessageChannel :: Text
      , _eMessageUser    :: Text
      , _eMessageText    :: Text
      , _eMessageTs      :: Text
      }
  deriving (Show)

deriveFromJSON
  defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "_eMessage" }
  ''EMessage
