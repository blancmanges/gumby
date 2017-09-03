{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.Slack.API.Web.Classes where

-- aeson
import           Data.Aeson (FromJSON)

-- wreq
import qualified Network.Wreq as Wr



class UrlEncode req where
  urlEncode :: req -> Wr.Options -> Wr.Options
  
  

class (UrlEncode req, FromJSON resp)
      => Method req resp
      | req -> resp
      where
  endpoint :: req -> String
