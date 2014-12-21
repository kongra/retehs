{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Flow
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-15
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
------------------------------------------------------------------------
module AI.Rete.Flow where

import           AI.Rete.Data
import           Control.Concurrent.STM
import           Control.Monad (when, liftM)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- GENERATING IDS
newtype GenidRead  = CGenidRead  EnvIdState
newtype GenidCheck = CGenidCheck ()
newtype GenidNew   = CGenidNew   Id
newtype GenidWrite = CGenidWrite ()
newtype Genid      = CGenid      Id

genidRead :: Env -> STM GenidRead
genidRead Env { envIdState = eid } = liftM CGenidRead (readTVar eid)
{-# INLINE genidRead #-}

genidCheck :: GenidRead -> STM GenidCheck
genidCheck (CGenidRead (EnvIdState recent)) = do
  -- Hopefully not in a reasonable time
  when (recent == maxBound) (error "Id overflow, can't go on.")
  return $! CGenidCheck ()
{-# INLINE genidCheck #-}

genidNew :: GenidRead -> GenidCheck -> GenidNew
genidNew (CGenidRead (EnvIdState recent)) _ = CGenidNew $! recent + 1
{-# INLINE genidNew #-}

genidWrite :: Env -> GenidNew -> STM GenidWrite
genidWrite Env { envIdState = eid } (CGenidNew new) = do
  writeTVar eid  $! EnvIdState new
  return $! CGenidWrite ()
{-# INLINE genidWrite #-}

genidResult :: GenidNew -> GenidWrite -> Genid
genidResult (CGenidNew new) _ = CGenid new
{-# INLINE genidResult #-}

-- | Generates a new Id.
genid :: Env -> STM Genid
genid env = do
  r      <- genidRead   env
  chk    <- genidCheck  r
  let new = genidNew    r   chk
  w      <- genidWrite  env new
  return $! genidResult new w
{-# INLINE genid #-}

-- ENVIRONMENT

-- | Creates a new, empty Env. The procedure is not tagged in any way
-- because it belongs to the API and there is no explicit dependency
-- on it.
createEnv :: STM Env
createEnv = do
  idState         <- newTVar $! EnvIdState         0
  symbolsRegistry <- newTVar $! EnvSymbolsRegistry Map.empty
  wmesRegistry    <- newTVar $! EnvWmesRegistry    Map.empty
  wmesByObj       <- newTVar $! EnvWmesByObj       Map.empty
  wmesByAttr      <- newTVar $! EnvWmesByAttr      Map.empty
  wmesByVal       <- newTVar $! EnvWmesByVal       Map.empty
  amems           <- newTVar $! EnvAmems           Map.empty
  prods           <- newTVar $! EnvProds           Set.empty

  return Env { envIdState         = idState
             , envSymbolsRegistry = symbolsRegistry
             , envWmesRegistry    = wmesRegistry
             , envWmesByObj       = wmesByObj
             , envWmesByAttr      = wmesByAttr
             , envWmesByVal       = wmesByVal
             , envAmems           = amems
             , envProds           = prods }
