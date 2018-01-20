{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Trans.Control.Extended (
    module Control.Monad.Trans.Control
  , controlLift2
) where

-- base
import           Control.Monad.IO.Class

-- monad-control
import           Control.Monad.Trans.Control (MonadBaseControl, StM, control)



controlLift2
  :: (MonadIO m, MonadBaseControl IO m)
  => (IO (StM m a) -> IO (StM m b) -> IO (StM m c)) -> m a -> m b -> m c
controlLift2 io2 ma mb = control $ \runInBase -> io2 (runInBase ma) (runInBase mb)