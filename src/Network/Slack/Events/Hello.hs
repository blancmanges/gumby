{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
module Network.Slack.Events.Hello where



-- aeson
import qualified Data.Aeson as Ae



-- https://api.slack.com/events/hello
data EHello
    = EHello {}
  deriving (Show, Eq)

instance Ae.FromJSON EHello where
    parseJSON = Ae.withObject "EHello" $ \_ -> pure EHello
