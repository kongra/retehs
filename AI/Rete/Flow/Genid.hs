{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Flow.Genid
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-22
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
------------------------------------------------------------------------
module AI.Rete.Flow.Genid
    (
      genid
    , Genid
    , genidVal
    )
    where

import AI.Rete.Data
import Control.Concurrent.STM
import Control.Monad (when, liftM)

-- GENERATING IDS
newtype GenidRead  = GenidRead  EnvIdState
newtype GenidCheck = GenidCheck ()
newtype GenidNew   = GenidNew   Id
newtype GenidWrite = GenidWrite ()
newtype Genid      = Genid      { genidVal :: Id }

genidRead :: Env -> STM GenidRead
genidRead Env { envIdState = eid } = liftM GenidRead (readTVar eid)
{-# INLINE genidRead #-}

genidCheck :: GenidRead -> STM GenidCheck
genidCheck (GenidRead (EnvIdState recent)) = do
  -- Hopefully not in a reasonable time
  when (recent == maxBound) (error "Id overflow, can't go on.")
  return $! GenidCheck ()
{-# INLINE genidCheck #-}

genidNew :: GenidRead -> GenidCheck -> GenidNew
genidNew (GenidRead (EnvIdState recent)) _ = GenidNew $! recent + 1
{-# INLINE genidNew #-}

genidWrite :: Env -> GenidNew -> STM GenidWrite
genidWrite Env { envIdState = eid } (GenidNew new) = do
  writeTVar eid  $! EnvIdState new
  return (GenidWrite ())
{-# INLINE genidWrite #-}

genidResult :: GenidNew -> GenidWrite -> Genid
genidResult (GenidNew new) _ = Genid { genidVal = new }
{-# INLINE genidResult #-}

-- | Generates a new Id.
genid :: Env -> STM Genid
genid env = do
  r      <- genidRead   env
  chk    <- genidCheck  r
  let new = genidNew    r   chk
  w      <- genidWrite  env new
  return (genidResult new w)
{-# INLINE genid #-}
