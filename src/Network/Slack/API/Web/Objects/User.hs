{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Slack.API.Web.Objects.User where

--
import           Data.Monoid.Nullable

-- text
import           Data.Text (Text)

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson (FromJSON, (.:))



data User
  = User
      { _userId       :: Text
      , _userName     :: Text
      , _userTz       :: Nullable Text
      , _userTzLabel  :: Text
      , _userTzOffset :: Int
      }
  deriving (Show)

instance FromJSON User where
  parseJSON = Ae.withObject "User" $ \user -> do
    _userId       <- user .: "id"
    _userName     <- user .: "name"
    _userTz       <- user .: "tz"
    _userTzLabel  <- user .: "tz_label"
    _userTzOffset <- user .: "tz_offset"
    pure User {..}
