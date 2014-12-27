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
import Control.Monad (when)

-- GENERATING IDS
newtype Genid = Genid { genidVal :: Id }

-- | Generates a new Id.
genid :: Env -> STM Genid
genid Env { envIdState = eid } = do
  EnvIdState recent <- readTVar eid

  -- Hopefully not in a reasonable time
  when (recent == maxBound) (error "Id overflow, can't go on.")

  let new = recent + 1
  writeTVar eid $! EnvIdState new
  return (Genid new)
{-# INLINE genid #-}
