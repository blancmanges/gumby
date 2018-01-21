{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Conf.Config where

-- (app)
import           App.Conf.LoggerData
import           App.Conf.DefaultableConf

-- base
import           Control.Concurrent

-- lens
import           Control.Lens hiding ((.=))

-- text
import           Data.Text (Text)



data WebsocketPingTime
instance GetDefault WebsocketPingTime Int where getDefault _ = 30

data RtmHostPort
instance GetDefault RtmHostPort Integer where getDefault _ = 443

data WebEndpoint
instance GetDefault WebEndpoint String where getDefault _ = "https://slack.com/api/"


-- |Config of the app.
data Config
  = Config
      -- App data
      { _cLoggingChan :: Chan LoggerData

      -- Required input
      , _cSecretSlackApiToken :: Text

      -- Optional input
      , _cWebsocketPingTime :: DefaultableConf Int     WebsocketPingTime
      , _cRtmHostPort       :: DefaultableConf Integer RtmHostPort
      , _cWebEndpoint       :: DefaultableConf String  WebEndpoint
      }

makeClassy ''Config
