{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Conf.RtmAppState where

-- (app)
import           App.Conf.Config (HasConfig(..))
import           App.Conf.AppState
import           App.Conf.DefaultableConf 

-- base
import           Control.Concurrent
import           Data.Void

-- bytestring
import qualified Data.ByteString.Lazy as BSL

-- lens
import           Control.Lens hiding ((.=))

-- text
import           Data.Text (Text)

-- wreq
import qualified Network.Wreq as Wr

-- websockets
import qualified Network.WebSockets as WS



-- |RTM app state. Includes config, app state, data from Slack, and other runtime state
-- for the bot.
data RtmAppState
  = RtmAppState
      { _rasAppState   :: AppState
      , _rasBotId      :: Text
      , _rasConn       :: WS.Connection
      , _rasChFromRtm  :: Chan BSL.ByteString
      , _rasChToRtm    :: Chan BSL.ByteString
      , _rasChToWeb    :: Chan (Wr.Options, String, Chan BSL.ByteString)
      , _rasChFromBots :: Chan Void
      , _rasChToBot    :: Chan BSL.ByteString
      }

makeClassy ''RtmAppState

instance HasConfig RtmAppState 'DUnwrapped where
    config = rasAppState . asConfig

instance HasAppState RtmAppState where
    appState = rasAppState