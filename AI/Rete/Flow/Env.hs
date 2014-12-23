{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Flow.Env
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-22
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
------------------------------------------------------------------------
module AI.Rete.Flow.Env
    (
      createEnv
    )
    where

import           AI.Rete.Data
import           Control.Concurrent.STM
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- | Creates a new, empty Env. The procedure is not tagged in any way
-- because it belongs to the API and there is no explicit dependency
-- on it.
createEnv :: STM Env
createEnv = do
  idState         <- newTVar $! EnvIdState         0
  symbolsRegistry <- newTVar $! EnvSymbolsRegistry Map.empty
  varsRegistry    <- newTVar $! EnvVarsRegistry    Map.empty
  wmesRegistry    <- newTVar $! EnvWmesRegistry    Map.empty
  wmesByObj       <- newTVar $! EnvWmesByObj       Map.empty
  wmesByAttr      <- newTVar $! EnvWmesByAttr      Map.empty
  wmesByVal       <- newTVar $! EnvWmesByVal       Map.empty
  amems           <- newTVar $! EnvAmems           Map.empty
  prods           <- newTVar $! EnvProds           Set.empty

  return Env { envIdState         = idState
             , envSymbolsRegistry = symbolsRegistry
             , envVarsRegistry    = varsRegistry
             , envWmesRegistry    = wmesRegistry
             , envWmesByObj       = wmesByObj
             , envWmesByAttr      = wmesByAttr
             , envWmesByVal       = wmesByVal
             , envAmems           = amems
             , envProds           = prods }
