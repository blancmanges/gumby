{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module App.Conf.BotState where

-- (lib)
import qualified Network.Slack.API.RTM.Request as Rtm
import qualified Network.Slack.API.RTM.Event   as Rtm
import qualified Network.Slack.API.Web.Classes as Web
-- (app)
import           App.Conf.RtmAppState

-- base
import           Control.Monad.IO.Class

-- text
import           Data.Text (Text)

-- lens
import           Control.Lens hiding ((.=))



data BotState
  = BotState
      { _botReallyReallyNeedsRtmAppState :: RtmAppState
      , _botCallout :: Text
      , _botRtmEventGetter :: forall m. MonadIO m => m (Either String Rtm.Event)
      , _botRtmRequestSender :: forall m. MonadIO m => Rtm.Request -> m ()
      , _botWebMethodCaller
          :: forall m req resp. (MonadIO m, Web.Method req resp)
          => req
          -> m (Either String resp)
      }

makeClassy ''BotState