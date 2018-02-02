{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Main (
    main
) where

-- (lib)
import qualified Network.Slack.API.RTM.Events.Message     as Rtm
import qualified Network.Slack.API.RTM.Request            as Rtm
import qualified Network.Slack.API.RTM.Lens               as Rtm
-- (app)
import           App.Bootstrap
import           App.Conf

-- base
import           Control.Monad
import           Control.Monad.IO.Class

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- lens
import           Control.Lens hiding ((.=))
import qualified Data.ByteString.Strict.Lens as BSS

-- time
import qualified Data.Time as Time

-- tzdata
import qualified Data.Time.Zones.DB as TimeZones

-- tz
import qualified Data.Time.Zones as TimeZones
import qualified Data.Time.Zones.All as TimeZones



main :: IO ()
main = bootstrapBot gumby



gumby :: forall m void. IsBotMonad m => m void
gumby = do
    (callout :: Text) <- view botCallout
    forever $ runSimpleCommand callout go
  where
    genericErrReply = "(intensively thinking in confusion)…        Hello!"

    go :: Rtm.EMessage -> Text -> m ()
    go msg (T.toLower -> help)
      | help == "help" || help == "list"
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = "Commands available:\n    `tz` - show current time in various time zones"
          }

    go msg (T.toLower -> hi)
      | hi == "hello" || hi == "hi" || hi == "ohai"
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = "Hello!"
          }

    go msg "tz"
      = do
          currentUtc <- liftIO $ Time.getCurrentTime

          let tzs = [ TimeZones.America__Los_Angeles
                    , TimeZones.America__Chicago
                    , TimeZones.America__New_York
                    , TimeZones.Etc__UCT
                    , TimeZones.Europe__Warsaw
                    , TimeZones.Europe__Saratov
                    ]

              asdf :: TimeZones.TZLabel -> (TimeZones.TZ, TimeZones.TZLabel)
              asdf tz = (TimeZones.tzByLabel tz, tz)

              tzForUTC :: (TimeZones.TZ, TimeZones.TZLabel) -> (Time.TimeZone, TimeZones.TZLabel)
              tzForUTC (tz, tzl) = (TimeZones.timeZoneForUTCTime tz currentUtc, tzl)

              zonedTime :: (Time.TimeZone, TimeZones.TZLabel) -> (Time.ZonedTime, TimeZones.TZLabel)
              zonedTime (zone, tzl) = (Time.utcToZonedTime zone currentUtc, tzl)

              formatTime :: (Time.ZonedTime, TimeZones.TZLabel) -> (String, String)
              formatTime (zoned, tzl) = (tzName, zonedFmt)
                where
                    tzName = tzl ^. to TimeZones.toTZName . BSS.unpackedChars
                    zonedFmt = Time.formatTime Time.defaultTimeLocale "   --   %a, %Y-%m-%d at %H:%M   --   %Z (%z)" zoned

              align :: [(String, String)] -> [(String, String, String)]
              align xs = fmap alignRow xs
                where
                    maxLocationLength = maximum . fmap (\(x,_) -> length x) $ xs
                    alignRow (x, y) = (x, replicate (maxLocationLength - length x) ' ', y)

              buildSlackMsg :: [(String, String, String)] -> T.Text
              buildSlackMsg = T.pack . (\body -> "```" ++ body ++ "```") . unlines . fmap (\(x,y,z) -> x ++ y ++ z)

              reply = buildSlackMsg
                    . align
                    . fmap (formatTime . zonedTime . tzForUTC . asdf)
                    $ tzs

          sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
            { Rtm._rSendMessageId = 999
            , Rtm._rSendMessageChannel = msg ^. Rtm.channel
            , Rtm._rSendMessageText = reply
            }


    go msg _
      = sendRtmRequest . Rtm.SendMessage $ Rtm.RSendMessage
          { Rtm._rSendMessageId = 123
          , Rtm._rSendMessageChannel = msg ^. Rtm.channel
          , Rtm._rSendMessageText = genericErrReply
          }

