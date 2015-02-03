{-# LANGUAGE    Trustworthy           #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    FlexibleInstances     #-}
{-# OPTIONS_GHC -W -Wall              #-}
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
import           Control.Monad (forM_, liftM)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as Seq
import           Safe (headMay)
-- -- import           Data.Hashable (Hashable)
-- -- import           Kask.Control.Monad (mapMM_, forMM_, toListM, whenM)
-- -- import           Kask.Data.Sequence (removeFirstOccurence)

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

-- BMEM CREATION

buildOrShareBmem :: Env -> Join -> STM Bmem
buildOrShareBmem env parent = do
  -- Search for an existing Bmem to share.
  sharedBmem <- readTVar (joinBmem parent)
  case sharedBmem of
    Just bmem -> return bmem  -- Happily found.
    Nothing   -> do
      -- Create new Bmem.
      id'         <- genid env
      children    <- newTVar Set.empty
      allChildren <- newTVar Set.empty
      toks        <- newTVar Set.empty

      let bmem = Bmem { bmemId          = id'
                      , bmemParent      = parent
                      , bmemChildren    = children
                      , bmemAllChildren = allChildren
                      , bmemToks        = toks }

      -- Set it in its parent.
      writeTVar (joinBmem parent) $! Just bmem

      updateFromAbove env bmem parent
      return bmem

-- PROCESSING CONDS FOR PRODUCTION CREATION

type IndexedPosCond = (Int, PosCond)
type IndexedNegCond = (Int, NegCond)

indexedPosConds :: [PosCond] -> [IndexedPosCond]
indexedPosConds = zip [0 ..]
{-# INLINE indexedPosConds #-}

indexedNegConds :: Int -> [NegCond] -> [IndexedNegCond]
indexedNegConds start = zip [start ..]
{-# INLINE indexedNegConds #-}

-- JOIN TESTS

-- | Returns a field within the PosCond that is equal to the passed
-- Symbol.
fieldEqualTo :: PosCond -> Symbol -> Maybe Field
fieldEqualTo (PosCond (Obj obj) (Attr attr) (Val val)) s
  | obj  == s = Just O
  | attr == s = Just A
  | val  == s = Just V
  | otherwise = Nothing
{-# INLINE fieldEqualTo #-}

matchingLocation :: Symbol -> IndexedPosCond -> Maybe Location
matchingLocation s (i, cond) = case fieldEqualTo cond s of
  Nothing -> Nothing
  Just f  -> Just (Location i f)
{-# INLINE matchingLocation #-}

joinTestForField :: Int -> Symbol -> Field -> [IndexedPosCond] -> Maybe JoinTest
joinTestForField i v field earlierConds
  | isVariable v =
    case headMay (matches earlierConds) of
      Nothing             -> Nothing
      Just (Location i' f) -> Just (JoinTest field f (i - i'))

  | otherwise = Nothing -- No tests from Consts (non-Vars).
  where
    matches = map fromJust . filter isJust . map (matchingLocation v)
{-# INLINE joinTestForField #-}

joinTestsForPosCond :: IndexedPosCond -> [IndexedPosCond] -> [JoinTest]
joinTestsForPosCond (i, PosCond obj attr val) =
  joinTestsForCondImpl i obj attr val
{-# INLINE joinTestsForPosCond #-}

joinTestsForNegCond :: IndexedNegCond -> [IndexedPosCond] -> [JoinTest]
joinTestsForNegCond (i, NegCond obj attr val) =
  joinTestsForCondImpl i obj attr val
{-# INLINE joinTestsForNegCond #-}

joinTestsForCondImpl :: Int -> Obj -> Attr -> Val
                     -> [IndexedPosCond] -> [JoinTest]
joinTestsForCondImpl i (Obj obj) (Attr attr) (Val val) earlierConds =
  result3
  where
    test1   = joinTestForField i obj  O earlierConds
    test2   = joinTestForField i attr A earlierConds
    test3   = joinTestForField i val  V earlierConds

    result1 = [fromJust test3 | isJust test3]
    result2 = if isJust test2 then fromJust test2 : result1 else result1
    result3 = if isJust test1 then fromJust test1 : result2 else result2
{-# INLINE joinTestsForCondImpl #-}

-- NEAREST ANCESTOR WITH THE SAME Amem.

class FindAncestor a b where findAncestor :: a -> Amem -> Maybe b

instance FindAncestor Join Join where
  findAncestor join amem = if joinAmem join == amem
    then Just join
    else findAncestor (joinParent join) amem

instance FindAncestor (Either Dtn Bmem) Join where
  findAncestor (Left  _   ) _    = Nothing
  findAncestor (Right bmem) amem = findAncestor (bmemParent bmem) amem

instance FindAncestor Neg AmemSuccessor where
  findAncestor neg amem = if negAmem neg == amem
    then Just (NegSuccessor neg)
    else findAncestor (negParent neg) amem

instance FindAncestor (Either Join Neg) AmemSuccessor where
  findAncestor (Left join) amem = liftM JoinSuccessor (findAncestor join amem)
  findAncestor (Right neg) amem = findAncestor neg amem

-- -- JOIN CREATION

-- -- buildOrShareJoinNode :: Env -> Node -> Amem -> [JoinTest] -> STM Node
-- -- buildOrShareJoinNode env parent amem tests = do

-- --   undefined

--   -- -- parent is always a β-memory, so below it's safe
--   -- allParentChildren <- rvprop bmemAllChildren parent
--   -- let matchingOneOf = headMay
--   --                     . filter (isShareableJoinNode amem tests)
--   --                     . Set.toList
--   -- case matchingOneOf allParentChildren of
--   --   Just node -> return node
--   --   Nothing   -> do
--   --     -- Establish the unlinking stuff ...
--   --     unlinkRight <- nullTSet (vprop nodeToks parent)
--   --     ru          <- newTVar unlinkRight
--   --     -- ... unlinking left only if the right ul. was not applied.
--   --     unlinkLeft'    <- nullTSet (amemWmes amem)
--   --     let unlinkLeft = not unlinkRight && unlinkLeft'
--   --     lu             <- newTVar unlinkLeft

--   --     -- Create new node with JoinNode variant
--   --     let ancestor = findNearestAncestorWithSameAmem parent amem
--   --     node <- newNode env parent
--   --             JoinNode { nodeAmem                    = amem
--   --                      , nearestAncestorWithSameAmem = ancestor
--   --                      , joinTests                   = tests
--   --                      , leftUnlinked                = lu
--   --                      , rightUnlinked               = ru }

--   --     -- Add node to parent.allChildren
--   --     writeTVar (vprop bmemAllChildren parent) $!
--   --       Set.insert node allParentChildren

--   --     -- Increment amem.reference-count
--   --     modifyTVar' (amemReferenceCount amem) (+1)

--   --     unless unlinkRight $
--   --       -- Insert node at the head of amem.successors
--   --       modifyTVar' (amemSuccessors amem) (node Seq.<|)

--   --     unless unlinkLeft $
--   --       -- Add node (to the head) of parent.children
--   --       modifyTVar' (nodeChildren parent) (node Seq.<|)

-- UPDATING NEW NODES WITH MATCHES FROM ABOVE

class UpdateFromAbove a p where updateFromAbove :: Env -> a -> p -> STM ()

instance UpdateFromAbove Bmem Join where updateFromAbove = undefined
