{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.Slack.Events where



-- base
import           Data.Maybe
import           Data.List

-- aeson
import qualified Data.Aeson as Ae
import           Data.Aeson ((.:?), (.:))
import qualified Data.Aeson.Types as Ae
import           Data.Aeson.Types (camelTo2)
import           Data.Aeson.TH (deriveFromJSON, defaultOptions, Options(..))

-- text
import qualified Data.Text as T

-- lens
import           Control.Lens



-- https://api.slack.com/events/hello
data EHello
  = EHello {}
  deriving (Show, Eq)

instance Ae.FromJSON EHello where
    parseJSON = Ae.withObject "EHello" $ \_ -> pure EHello



-- https://api.slack.com/events/message
data EMessage
  = EMessage
      { _eMessageChannel :: T.Text
      , _eMessageUser :: T.Text
      , _eMessageText :: T.Text
      , _eMessageTs :: T.Text
      }
  deriving (Show, Eq)
makeFields ''EMessage

deriveFromJSON
    defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "_eMessage" }
    ''EMessage



-- https://api.slack.com/rtm#handling_responses
data EMessageSent
  = EMessageSent
      { _eMessageSentOk :: Bool
      , _eMessageSentReplyTo :: Int
      , _eMessageSentTs :: T.Text
      , _eMessageSentText :: T.Text
      }
  deriving (Show, Eq)
makeFields ''EMessageSent

deriveFromJSON
    defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "_eMessageSent" }
    ''EMessageSent



-- https://api.slack.com/events/reconnect_url
data EReconnectUrl
  = EReconnectUrl {}
  deriving (Show, Eq)

instance Ae.FromJSON EReconnectUrl where
    parseJSON = Ae.withObject "EReconnectUrl" $ \_ -> pure EReconnectUrl



-- https://api.slack.com/events/presence_change
data EPresenceChange
  = EPresenceChange
      { _ePresenceChangeUsers :: [T.Text]
      , _ePresenceChangePresence :: T.Text  -- TODO: enum field
      }
  deriving (Show, Eq)

instance Ae.FromJSON EPresenceChange where
    parseJSON = Ae.withObject "EPresenceChange" $ \o -> do
        usersM <- o .:? "users"
        presence <- o .: "presence"
        case usersM of
            Just users ->
                pure $ EPresenceChange users presence
            Nothing -> do
                user <- o .: "user"
                pure $ EPresenceChange [user] presence
    


-- https://api.slack.com/events/user_typing
data EUserTyping
  = EUserTyping
      { _eUserTypingChannel :: T.Text
      , _eUserTypingUser :: T.Text
      }
  deriving (Show, Eq)
makeFields ''EUserTyping

deriveFromJSON
  defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "_eUserTyping" }
  ''EUserTyping



data Event
  = Hello EHello
  | MessageSent EMessageSent
  | PresenceChange EPresenceChange
  | ReconnectUrl EReconnectUrl
  | UserTyping EUserTyping
  | Message EMessage
  | UnknownEvent
  deriving (Show, Eq)
makeLenses ''Event

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
