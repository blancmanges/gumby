{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.API.RTM.Request where


-- base
import           Prelude hiding (id)

-- text
import           Data.Text (Text)

-- aeson
import           Data.Aeson (object, ToJSON(..), (.=))



data RSendMessage
  = RSendMessage
      { _rSendMessageId      :: Int
      , _rSendMessageChannel :: Text
      , _rSendMessageText    :: Text
      }
  deriving (Show)

instance ToJSON RSendMessage where
  toJSON x = object [ "id"      .= _rSendMessageId x
                    , "type"    .= ("message" :: Text)
                    , "channel" .= _rSendMessageChannel x
                    , "text"    .= _rSendMessageText x
                    ]



data Request
  = SendMessage RSendMessage
  deriving (Show)

instance ToJSON Request where
  toJSON req = case req of
      SendMessage x -> toJSON x
