{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.EventsSpec where



-- gumby
import           Network.Slack.Events

-- aeson
import qualified Data.Aeson as Ae

-- hspec + gumby
import           Test.Hspec.Expectations.Aeson

-- neat-interpolation
import qualified NeatInterpolation as Multiline



spec :: Spec
spec = do
    -- -------------------------------------------------------------------------
    describe "events/hello" $ do
        let exampleEventText =
                [Multiline.text|
                    {
                        "type": "hello"
                    }
                |]
            exampleEvent =
                EHello {}

        it "instance FromJSON EHello" $ do
            exampleEventText `shouldDecodeTo` exampleEvent
        it "decodes to proper event" $ do
            exampleEventText `shouldDecodeTo` Hello exampleEvent
    
    -- -------------------------------------------------------------------------
    describe "send message confirmation" $ do
        let exampleEventText = 
                [Multiline.text|
                    {
                        "ok": true,
                        "reply_to": 1,
                        "ts": "1355517523.000005",
                        "text": "Hello world"
                    }
                |]
            exampleEvent =
                EMessageSent
                    { _eMessageSentOk = True
                    , _eMessageSentReplyTo = 1
                    , _eMessageSentTs = "1355517523.000005"
                    , _eMessageSentText = "Hello world"
                    }

        it "instance FromJSON EMessageSent" $ do
            exampleEventText `shouldDecodeTo` exampleEvent
        it "decodes to proper event" $ do
            exampleEventText `shouldDecodeTo` MessageSent exampleEvent

    -- -------------------------------------------------------------------------
    describe "events/presence_change" $ do
        let exampleEvent1Text = 
                [Multiline.text|
                    {
                        "type": "presence_change",
                        "user": "U024BE7LH",
                        "presence": "away"
                    }
                |]
            exampleEvent1 =
                EPresenceChange
                    { _ePresenceChangeUsers = ["U024BE7LH"]
                    , _ePresenceChangePresence = "away"
                    }
            
            exampleEvent2Text = 
                [Multiline.text|
                    {
                        "type": "presence_change_batch",
                        "users": ["U024BE7LH", "U012EA2U1"],
                        "presence": "away"
                    }
                |]
            exampleEvent2 =
                EPresenceChange
                    { _ePresenceChangeUsers = ["U024BE7LH", "U012EA2U1"]
                    , _ePresenceChangePresence = "away"
                    }

        it "instance FromJSON EPresenceChange" $ do
            exampleEvent1Text `shouldDecodeTo` exampleEvent1
            exampleEvent2Text `shouldDecodeTo` exampleEvent2
        it "decodes to proper event" $ do
            exampleEvent1Text `shouldDecodeTo` PresenceChange exampleEvent1
            exampleEvent2Text `shouldDecodeTo` PresenceChange exampleEvent2

    -- -------------------------------------------------------------------------
    describe "events/reconnect_url" $ do
        let exampleEventText =
                [Multiline.text|
                    {
                        "type": "reconnect_url"
                    }
                |]
            exampleEvent =
                EReconnectUrl {}
            
        it "instance FromJSON EPresenceChange" $ do
            exampleEventText `shouldDecodeTo` exampleEvent
        it "decodes to proper event" $ do
            exampleEventText `shouldDecodeTo` ReconnectUrl exampleEvent

    -- -------------------------------------------------------------------------
    describe "events/user_eUserTyping" $ do
        let exampleEventText =
                [Multiline.text|
                    {
                        "type": "user_typing",
                        "channel": "C02ELGNBH",
                        "user": "U024BE7LH"
                    }
                |]
            exampleEvent =
                EUserTyping
                    { _eUserTypingChannel = "C02ELGNBH"
                    , _eUserTypingUser = "U024BE7LH"
                    }
            
        it "instance FromJSON EUserTyping" $ do
            exampleEventText `shouldDecodeTo` exampleEvent
        it "decodes to proper event" $ do
            exampleEventText `shouldDecodeTo` UserTyping exampleEvent
    
    -- -------------------------------------------------------------------------
    describe "events/message" $ do
        let exampleEventText =
                [Multiline.text|
                    {
                        "type": "message",
                        "channel": "C2147483705",
                        "user": "U2147483697",
                        "text": "Hello world",
                        "ts": "1355517523.000005"
                    }
                |]
            exampleEvent =
                EMessage
                    { _eMessageChannel = "C2147483705"
                    , _eMessageUser = "U2147483697"
                    , _eMessageText = "Hello world"
                    , _eMessageTs = "1355517523.000005"
                    }
            
        it "instance FromJSON EMessage" $ do
            exampleEventText `shouldDecodeTo` exampleEvent
        it "decodes to proper event" $ do
            exampleEventText `shouldDecodeTo` Message exampleEvent