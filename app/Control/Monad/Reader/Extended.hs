{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Monad.Reader.Extended (
    module Control.Monad.Reader
  , runReaderTWith
) where

-- mtl
import           Control.Monad.Reader



runReaderTWith :: r -> ReaderT r m a -> m a
runReaderTWith = flip runReaderT