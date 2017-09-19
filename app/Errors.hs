{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}

module Errors where

-- base
import           Data.Monoid

-- text-show
import           TextShow (TextShow(showb))
import qualified TextShow as TS

-- bytestring
import qualified Data.ByteString.Lazy as BSL



data GumbyAppErr
  = EnvVarMissing String
  | JsonDecodeError String BSL.ByteString

instance TextShow GumbyAppErr where
  showb (EnvVarMissing varName)
    = TS.fromText "Missing environment variable: " <> showb varName
  showb (JsonDecodeError err originalJSON)
    = TS.fromText "Error while decoding JSON: "
        <> showb err
        <> ". The original JSON: " <> showb originalJSON