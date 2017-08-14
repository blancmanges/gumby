{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Slack.Events (
    Event(..)
  , module ReexportEvents
) where



-- gumby
import           Network.Slack.Events.Hello          as ReexportEvents
import           Network.Slack.Events.MessageSent    as ReexportEvents
import           Network.Slack.Events.PresenceChange as ReexportEvents
import           Network.Slack.Events.ReconnectUrl   as ReexportEvents
import           Network.Slack.Events.UserTyping     as ReexportEvents
import           Network.Slack.Events.Message        as ReexportEvents

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson ((.:?))
import qualified Data.Aeson.Types as Ae



data Event
    = Hello EHello
    | MessageSent EMessageSent
    | PresenceChange EPresenceChange
    | ReconnectUrl EReconnectUrl
    | UserTyping EUserTyping
    | Message EMessage
    | UnknownEvent
  deriving (Show, Eq)

instance Ae.FromJSON Event where
    parseJSON oo@(Ae.Object o) = do
        (mType :: Maybe String) <- o .:? "type"
        case mType of
            Just "hello"                 -> Hello          <$> continue
            Just "presence_change"       -> PresenceChange <$> continue
            Just "presence_change_batch" -> PresenceChange <$> continue
            Just "reconnect_url"         -> ReconnectUrl   <$> continue
            Just "user_typing"           -> UserTyping     <$> continue
            Nothing                      -> MessageSent    <$> continue
            Just "message"               -> Message        <$> continue
            Just "desktop_notification"  -> pure UnknownEvent  -- undocumented
            Just _                       -> pure UnknownEvent
      where
        continue :: Ae.FromJSON a => Ae.Parser a
        continue = Ae.parseJSON oo
