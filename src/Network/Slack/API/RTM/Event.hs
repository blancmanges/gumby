{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Slack.API.RTM.Event where


--
import           Network.Slack.API.RTM.Events.Message

-- text
import           Data.Text (Text)

-- aeson
import           Data.Aeson (FromJSON(..), Value(Object), (.:?))
import           Data.Aeson.Types (Parser)



data Event
  = Message EMessage
  | Unrecognized
  deriving (Show)

instance FromJSON Event where
  parseJSON input@(Object o) = do
      (mType :: Maybe Text) <- o .:? "type"
      case mType of
        Just "message" -> Message <$> continue
        _              -> pure Unrecognized
    where
      continue :: FromJSON a => Parser a
      continue = parseJSON input
