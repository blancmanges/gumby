{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Events.MessageSent where



-- gumby
import           Gumby.Helpers (lensField_to_underscores)

-- aeson
import qualified Data.Aeson.TH as AeTH

-- text
import qualified Data.Text as T



-- https://api.slack.com/rtm#handling_responses
data EMessageSent
    = EMessageSent
        { _ackOk :: Bool
        , _ackReplyTo :: Int
        , _ackTs :: T.Text
        , _ackText :: T.Text
        }
  deriving (Show, Eq)


AeTH.deriveFromJSON
    AeTH.defaultOptions { AeTH.fieldLabelModifier = lensField_to_underscores "_ack" }
    ''EMessageSent
