{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
module Network.Slack.Events.ReconnectUrl where
    
    
    
-- aeson
import qualified Data.Aeson as Ae



-- https://api.slack.com/events/reconnect_url
data EReconnectUrl
    = EReconnectUrl {}
    deriving (Show, Eq)

instance Ae.FromJSON EReconnectUrl where
    parseJSON = Ae.withObject "EReconnectUrl" $ \_ -> pure EReconnectUrl
    