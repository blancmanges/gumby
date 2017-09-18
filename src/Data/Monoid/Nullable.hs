{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Data.Monoid.Nullable where

-- aeson
import           Data.Aeson (FromJSON(parseJSON), Result(Success), fromJSON)



newtype Nullable a = Nullable (Maybe a)
  deriving (Show)

instance FromJSON a => FromJSON (Nullable a) where
  parseJSON o =
      case fromJSON o of
        Success a -> pure . Nullable $ Just a
        _         -> pure . Nullable $ Nothing
