{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Events.Message where



-- gumby
import           Gumby.Helpers (lensField_to_underscores)

-- aeson
import qualified Data.Aeson.TH as AeTH

-- text
import qualified Data.Text as T

-- lens
import           Control.Lens



-- https://api.slack.com/events/message
data EMessage
    = EMessage
        { _msgChannel :: T.Text
        , _msgUser :: T.Text
        , _msgText :: T.Text
        , _msgTs :: T.Text
        }
  deriving (Show, Eq)
makeLenses ''EMessage


AeTH.deriveFromJSON
    AeTH.defaultOptions { AeTH.fieldLabelModifier = lensField_to_underscores "_msg" }
    ''EMessage
