{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Monad.Trans.Maybe.Extended (
    module Control.Monad.Trans.Maybe
  , liftMaybe
) where

-- transformers
import           Control.Monad.Trans.Maybe

liftMaybe :: (Applicative m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure