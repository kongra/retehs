{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Net
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-01-19
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
------------------------------------------------------------------------
module AI.Rete.Net where

import           AI.Rete.Data
import           AI.Rete.Flow
import           Control.Concurrent.STM
import           Control.Monad (forM_)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
-- import           Data.Hashable (Hashable)
-- import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
-- import           Kask.Control.Monad (mapMM_, forMM_, toListM, whenM)
-- import           Kask.Data.Sequence (removeFirstOccurence)

isVariable :: Symbol -> Bool
isVariable (Var   _) = True
isVariable (Const _) = False
{-# INLINE isVariable #-}

-- AMEM CREATION

-- | Searches for an existing alpha memory for the given symbols or
-- creates a new one.
buildOrShareAmem :: Env -> Symbol -> Symbol -> Symbol -> STM Amem
buildOrShareAmem env obj attr val = do
  let w     = Const wildcardConstant
      obj'  = Obj  $ if isVariable obj  then w else obj
      attr' = Attr $ if isVariable attr then w else attr
      val'  = Val  $ if isVariable val  then w else val

  amems <- readTVar (envAmems env)
  let k = WmeKey obj' attr' val'

  case Map.lookup k amems of
    Just amem -> return amem -- Happily found.
    Nothing   -> do
      -- Let's create new Amem.
      successors <- newTVar Seq.empty
      refCount   <- newTVar 0

      wmes       <- newTVar Set.empty
      wmesByObj  <- newTVar Map.empty
      wmesByAttr <- newTVar Map.empty
      wmesByVal  <- newTVar Map.empty

      let amem = Amem { amemObj            = obj'
                      , amemAttr           = attr'
                      , amemVal            = val'
                      , amemSuccessors     = successors
                      , amemReferenceCount = refCount
                      , amemWmes           = wmes
                      , amemWmesByObj      = wmesByObj
                      , amemWmesByAttr     = wmesByAttr
                      , amemWmesByVal      = wmesByVal }

      -- Put amem into the env registry of Amems.
      writeTVar (envAmems env) $! Map.insert k amem amems

      activateAmemOnCreation env amem obj' attr' val'
      return amem

-- | A simplified, more effective version of amem activation that
-- takes place on the amem creation. No successors activation here,
-- cause no successors present.
activateAmemOnCreation :: Env -> Amem -> Obj -> Attr -> Val -> STM ()
activateAmemOnCreation env amem obj attr val = do
  byObjIndex  <- readTVar (envWmesByObj  env)
  byAttrIndex <- readTVar (envWmesByAttr env)
  byValIndex  <- readTVar (envWmesByVal  env)

  let wmesMatchingByObj  = Map.lookupDefault Set.empty obj  byObjIndex
      wmesMatchingByAttr = Map.lookupDefault Set.empty attr byAttrIndex
      wmesMatchingByVal  = Map.lookupDefault Set.empty val  byValIndex
      wmesMatching       = wmesMatchingByObj  `Set.intersection`
                           wmesMatchingByAttr `Set.intersection`
                           wmesMatchingByVal

  -- Put all matching wmes into the amem.
  writeTVar (amemWmes amem) wmesMatching

  -- Iteratively work on every wme.
  forM_ (toList wmesMatching) $ \wme -> do
    -- Put amem to wme registry of Amems.
    modifyTVar' (wmeAmems wme) (amem:)

    -- Put wme into amem indexes
    modifyTVar' (amemWmesByObj  amem) (wmesIndexInsert (wmeObj  wme) wme)
    modifyTVar' (amemWmesByAttr amem) (wmesIndexInsert (wmeAttr wme) wme)
    modifyTVar' (amemWmesByVal  amem) (wmesIndexInsert (wmeVal  wme) wme)
{-# INLINE activateAmemOnCreation #-}

-- ABSTRACTION FOR NETWORK CONSTRUCTION

-- | A generic node used as intermediate data-structure during the
-- network creation.
data ReteNode = ReteDtn
              | ReteJoin !Join
              | ReteNeg  !Neg
              | ReteNcc  !Ncc

class AddChild a where addChild :: ReteNode -> a -> STM ()

toTSeqFront :: TVar (Seq.Seq a) -> a -> STM ()
toTSeqFront s = modifyTVar' s . (Seq.<|)
{-# INLINE toTSeqFront #-}

-- BMEM CREATION

buildOrShareBmem :: Env -> ReteNode -> STM Bmem
buildOrShareBmem env parent = do
  sharedBem <- findChildBmem parent
  case sharedBem of
    Just bmem -> return bmem
    Nothing   -> do
      -- Create new Bmem.
      id'         <- genid env
      children    <- newTVar Seq.empty
      allChildren <- newTVar Seq.empty
      toks        <- newTVar Set.empty

      let bmem = Bmem { bmemId          = id'
                      , bmemParent      = bmemParentOf parent
                      , bmemChildren    = children
                      , bmemAllChildren = allChildren
                      , bmemToks        = toks }

      addChild                   parent bmem
      updateWithMatchesFromAbove env    bmem
      return bmem

findChildBmem :: ReteNode -> STM (Maybe Bmem)
findChildBmem = undefined

bmemParentOf :: ReteNode -> BmemParent
bmemParentOf (ReteJoin join) = JoinBmemParent join
bmemParentOf (ReteNeg  neg)  = NegBmemParent  neg
bmemParentOf (ReteNcc  ncc)  = NccBmemParent  ncc
bmemParentOf ReteDtn         = error "PANIC (6): Dtn MUST NOT BE A Bmem PARENT."
{-# INLINE bmemParentOf #-}

instance AddChild Bmem where
  addChild (ReteJoin join) = toTSeqFront (joinChildren join) . BmemJoinChild
  addChild (ReteNeg  neg)  = toTSeqFront (negChildren  neg)  . BmemNegChild
  addChild (ReteNcc  ncc)  = toTSeqFront (nccChildren  ncc)  . BmemNccChild
  addChild ReteDtn         = error "PANIC (7): Bmem MUST NOT BE A Dtn CHILD."

-- UPDATING NEW NODES WITH MATCHES FROM ABOVE

updateWithMatchesFromAbove :: Env -> a -> STM ()
updateWithMatchesFromAbove = undefined
