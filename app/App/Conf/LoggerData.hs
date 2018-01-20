{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module App.Conf.LoggerData where

-- monad-logger
import           Control.Monad.Logger


-- |The type used by monad-logger for inter-process logging.
type LoggerData = (Loc, LogSource, LogLevel, LogStr)
