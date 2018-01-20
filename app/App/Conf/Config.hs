{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}

module App.Conf.Config where

-- (app)
import           App.Conf.LoggerData

-- base
import           Text.Read
import           Control.Concurrent

-- data-default
import           Data.Default (Default, def)

-- lens
import           Control.Lens hiding ((.=))

-- text
import           Data.Text (Text)



newtype WebsocketPingTime = WebsocketPingTime { _unwrap :: Int }

instance Read WebsocketPingTime where
    readPrec = WebsocketPingTime <$> readPrec
instance Default WebsocketPingTime where
    def = WebsocketPingTime 30

makeClassy ''WebsocketPingTime
makePrisms ''WebsocketPingTime


-- |Config of the app.
data Config
  = Config
      { _cSecretSlackApiToken :: Text
      , _cLoggingChan :: Chan LoggerData
      , _cWebsocketPingTime :: WebsocketPingTime
      }

makeClassy ''Config
