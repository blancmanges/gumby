{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module App.Conf.AppState where

-- (app)
import           App.Conf.Config
import           App.Conf.DefaultableConf 

-- lens
import           Control.Lens hiding ((.=))



-- |App state. Includes config and app state.
data AppState
  = AppState
      { _asConfig      :: Config 'DUnwrapped
      }

makeClassy ''AppState

instance HasConfig AppState 'DUnwrapped where
    config = asConfig
