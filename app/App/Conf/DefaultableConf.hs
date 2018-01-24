{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module App.Conf.DefaultableConf where

-- base
import           Text.Read
import           Data.Proxy

-- data-default
import           Data.Default (Default(def))

-- lens
import           Control.Lens hiding ((.=))



data DefaultsWrapped
  = DWrapped
  | DUnwrapped


class GetDefault a b | a -> b where
    getDefault :: Proxy a -> b


type family DefaultableConfW isWrapped a b where
    DefaultableConfW 'DWrapped a b = DefaultableConf a b
    DefaultableConfW 'DUnwrapped a b = a

newtype DefaultableConf a b = DefaultableConf { _unwrapDefaultableConf :: a }

instance (Read a) => Read (DefaultableConf a b) where
    readPrec = DefaultableConf <$> readPrec
instance (GetDefault b a) => Default (DefaultableConf a b) where
    def = DefaultableConf $ getDefault (Proxy :: Proxy b)

makeClassy ''DefaultableConf