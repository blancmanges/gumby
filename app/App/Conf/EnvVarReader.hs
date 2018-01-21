{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Conf.EnvVarReader where

-- (app)
import           Errors
import           App.Conf.DefaultableConf

-- base
import           Control.Monad.IO.Class
import           System.Environment (lookupEnv)
import           Text.Read

-- text-show
import           TextShow (TextShow, showt)
import qualified TextShow as TS

-- mtl
import           Control.Monad.Except (MonadError, throwError)

-- monad-logger
import           Control.Monad.Logger (MonadLogger, logDebug, logError)

-- data-default
import           Data.Default (def)

-- lens
import           Control.Lens hiding ((.=))



getEnvVarWithDefaults
    :: (MonadIO m, TextShow a, GetDefault b a, MonadLogger m, Read a, MonadError GumbyAppErr m)
    => String -> m (DefaultableConf a b)
getEnvVarWithDefaults envVarKey = do
    query <- liftIO $ lookupEnv envVarKey
    case query of

        Nothing -> do
            let val = def
                unwrapped = val ^. unwrapDefaultableConf
                logmsg = [ TS.fromText "Env var"
                         , TS.fromString envVarKey
                         , TS.fromText "does not exist, using default value:"
                         , TS.fromText . showt $ unwrapped
                         ]
            $(logDebug) (TS.toText $ TS.unwordsB logmsg)
            pure val

        Just envVarValue -> do
            case readMaybe envVarValue of
                Just parsed -> do
                    let logmsg = [ TS.fromText "Reading"
                                 , TS.fromString envVarKey
                                 , TS.fromText "from env variables:"
                                 , TS.fromText . showt $ parsed]
                        wrapped = DefaultableConf parsed
                    $(logDebug) (TS.toText $ TS.unwordsB logmsg)
                    pure wrapped

                Nothing -> do
                    let logmsg = [ TS.fromText "Env var"
                                 , TS.fromString envVarKey
                                 , TS.fromText "cannot be converted to proper value:"
                                 , TS.fromString envVarValue
                                 ]
                    $(logError) (TS.toText $ TS.unwordsB logmsg)
                    throwError $ EnvVarUnreadable envVarKey
