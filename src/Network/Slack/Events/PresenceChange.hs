{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Slack.Events.PresenceChange where



-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson ((.:), (.:?))

-- text
import qualified Data.Text as T



-- https://api.slack.com/events/presence_change
data EPresenceChange
    = EPresenceChange
        { _presenceUsers :: [T.Text]
        , _presencePresence :: T.Text  -- TODO: enum field
        }
  deriving (Show, Eq)


instance Ae.FromJSON EPresenceChange where
    parseJSON = Ae.withObject "EPresenceChange" $ \o -> do
        usersM <- o .:? "users"
        case usersM of
            Just users ->
                EPresenceChange users <$> o .: "presence"
            Nothing -> do
                user <- o .: "user"
                EPresenceChange [user] <$> o .: "presence"
