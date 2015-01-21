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
import           Data.List (sortBy)
import           Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as Seq
import           Safe (headMay)
-- import           Data.Hashable (Hashable)
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
data ReteNode = ReteDtn  !Dtn
              | ReteJoin !Join
              | ReteNeg  !Neg
              | ReteNcc  !Ncc

class AddChild a where addChild :: ReteNode -> a -> STM ()

toTSeqFront :: TVar (Seq.Seq a) -> a -> STM ()
toTSeqFront s = modifyTVar' s . (Seq.<|)
{-# INLINE toTSeqFront #-}

-- | A generic representation of a parent node.
data ParentNode = ParentDtn  !Dtn
                | ParentBmem !Bmem
                | ParentJoin !Join
                | ParentNeg  !Neg
                | ParentNcc  !Ncc

class WithParentNode a where parentNode :: a -> Maybe ParentNode

instance WithParentNode ParentNode where
  parentNode (ParentDtn  _   ) = Nothing
  parentNode (ParentBmem bmem) = parentNode bmem
  parentNode (ParentJoin join) = parentNode join
  parentNode (ParentNeg  neg)  = parentNode neg
  parentNode (ParentNcc  ncc)  = parentNode ncc

instance WithParentNode Bmem    where parentNode = undefined
instance WithParentNode Join    where parentNode = undefined
instance WithParentNode Neg     where parentNode = undefined
instance WithParentNode Ncc     where parentNode = undefined
instance WithParentNode Partner where parentNode = undefined

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
bmemParentOf (ReteDtn  _  )  = error "PANIC (6): Dtn MUST NOT BE A Bmem PARENT."
{-# INLINE bmemParentOf #-}

instance AddChild Bmem where
  addChild (ReteJoin join) = toTSeqFront (joinChildren join) . BmemJoinChild
  addChild (ReteNeg  neg)  = toTSeqFront (negChildren  neg)  . BmemNegChild
  addChild (ReteNcc  ncc)  = toTSeqFront (nccChildren  ncc)  . BmemNccChild
  addChild (ReteDtn  _  )  = error "PANIC (7): Bmem MUST NOT BE A Dtn CHILD."

-- PROCESSING CONDS

-- | It is desirable for the Conds of a production to be sorted in
-- such a way that the positive conds come before the negative and the
-- ncc conds. Also the subconditions of a ncc should be sorted in such
-- way. The following procedure does the job.
sortConds :: [Cond] -> [Cond]
sortConds = map processCond . sortBy (flip condsOrdering)
  where
    processCond c = case c of
      (NccCond subconds) -> NccCond (sortConds subconds)
      _                  -> c
{-# INLINE sortConds #-}

condsOrdering :: Cond -> Cond -> Ordering
condsOrdering PosCond {} PosCond {} = EQ
condsOrdering PosCond {} _          = GT

condsOrdering NegCond {} PosCond {} = LT
condsOrdering NegCond {} NegCond {} = EQ
condsOrdering NegCond {} NccCond {} = GT

condsOrdering NccCond {} PosCond {} = LT
condsOrdering NccCond {} NegCond {} = LT
condsOrdering NccCond {} NccCond {} = EQ
{-# INLINE condsOrdering #-}

-- JOIN TESTS

-- | Returns a field within the PosCond that is equal to the passed
-- Symbol.
fieldEqualTo :: Cond -> Symbol -> Maybe Field
fieldEqualTo (PosCond obj attr val) s
  | obj  == Obj  s = Just O
  | attr == Attr s = Just A
  | val  == Val  s = Just V
  | otherwise      = Nothing
fieldEqualTo _ _  = error "PANIC (8): ONLY PosConds ALLOWED HERE."
{-# INLINE fieldEqualTo #-}

type IndexedCond  = (Cond, Distance)
data IndexedField = IndexedField !Field !Distance

indexedPositiveConds :: [Cond] -> [IndexedCond]
indexedPositiveConds conds =
  filter positive (zip (reverse conds) [0 ..])
  where
    positive (PosCond {}, _) = True
    positive _               = False
{-# INLINE indexedPositiveConds #-}

indexedField :: Symbol -> IndexedCond -> Maybe IndexedField
indexedField s (cond, d) = case fieldEqualTo cond s of
  Nothing -> Nothing
  Just f  -> Just (IndexedField f d)

joinTestFromField :: Symbol -> Field -> [IndexedCond] -> Maybe JoinTest
joinTestFromField v field earlierConds
  | isVariable v =
    case headMay (matches earlierConds) of
      Nothing                 -> Nothing
      -- Indices are 0-based. When creating a JoinTest we have to
      -- increase it to maintain a required 1-based indexing.
      Just (IndexedField f d) -> Just (JoinTest field f (d+1))

  | otherwise = Nothing -- No tests from Consts (non-Vars).
  where
    matches = map fromJust . filter isJust . map (indexedField v)
{-# INLINE joinTestFromField #-}

-- | Extracts and returns join tests for the given condition.
joinTestsFromCond :: Cond -> [Cond] -> [JoinTest]
joinTestsFromCond (PosCond obj attr val) earlierConds =
  joinTestsFromCondImpl obj attr val earlierConds
joinTestsFromCond (NegCond obj attr val) earlierConds =
  joinTestsFromCondImpl obj attr val earlierConds
joinTestsFromCond cond@_ _ =
  error ("PANIC (9): ONLY PosCond OR NegCond ALLOWED HERE, GIVEN " ++ show cond)
{-# INLINE joinTestsFromCond #-}

joinTestsFromCondImpl :: Obj -> Attr -> Val -> [Cond] -> [JoinTest]
joinTestsFromCondImpl (Obj obj) (Attr attr) (Val val) earlierConds = result3
  where
    econds  = indexedPositiveConds earlierConds
    test1   = joinTestFromField    obj  O econds
    test2   = joinTestFromField    attr A econds
    test3   = joinTestFromField    val  V econds

    result1 = [fromJust test3 | isJust test3]
    result2 = if isJust test2 then fromJust test2 : result1 else result1
    result3 = if isJust test1 then fromJust test1 : result2 else result2
{-# INLINE joinTestsFromCondImpl #-}

-- NEAREST ANCESTOR WITH THE SAME Amem.

data NodeWithAmem = JoinWithAmem !Join
                  | NegWithAmem  !Neg

-- | Returns the nearest ancestor with the passed Amem.
nearestAncestor :: Maybe ParentNode -> Amem -> Maybe NodeWithAmem
nearestAncestor Nothing       _    = Nothing
nearestAncestor (Just parent) amem = case parent of
  ParentJoin join
    -> if joinAmem join == amem
       then Just $! JoinWithAmem join
       else nearestAncestor (parentNode parent) amem

  ParentNeg neg
    -> if negAmem neg == amem
       then Just $! NegWithAmem neg
       else nearestAncestor (parentNode parent) amem

  ParentNcc ncc
    -> nearestAncestor (parentNode (nccPartner ncc)) amem

  _ -> nearestAncestor (parentNode parent) amem

-- JOIN CREATION

-- buildOrShareJoinNode :: Env -> Node -> Amem -> [JoinTest] -> STM Node
-- buildOrShareJoinNode env parent amem tests = do

--   undefined

  -- -- parent is always a β-memory, so below it's safe
  -- allParentChildren <- rvprop bmemAllChildren parent
  -- let matchingOneOf = headMay
  --                     . filter (isShareableJoinNode amem tests)
  --                     . Set.toList
  -- case matchingOneOf allParentChildren of
  --   Just node -> return node
  --   Nothing   -> do
  --     -- Establish the unlinking stuff ...
  --     unlinkRight <- nullTSet (vprop nodeToks parent)
  --     ru          <- newTVar unlinkRight
  --     -- ... unlinking left only if the right ul. was not applied.
  --     unlinkLeft'    <- nullTSet (amemWmes amem)
  --     let unlinkLeft = not unlinkRight && unlinkLeft'
  --     lu             <- newTVar unlinkLeft

  --     -- Create new node with JoinNode variant
  --     let ancestor = findNearestAncestorWithSameAmem parent amem
  --     node <- newNode env parent
  --             JoinNode { nodeAmem                    = amem
  --                      , nearestAncestorWithSameAmem = ancestor
  --                      , joinTests                   = tests
  --                      , leftUnlinked                = lu
  --                      , rightUnlinked               = ru }

  --     -- Add node to parent.allChildren
  --     writeTVar (vprop bmemAllChildren parent) $!
  --       Set.insert node allParentChildren

  --     -- Increment amem.reference-count
  --     modifyTVar' (amemReferenceCount amem) (+1)

  --     unless unlinkRight $
  --       -- Insert node at the head of amem.successors
  --       modifyTVar' (amemSuccessors amem) (node Seq.<|)

  --     unless unlinkLeft $
  --       -- Add node (to the head) of parent.children
  --       modifyTVar' (nodeChildren parent) (node Seq.<|)




-- UPDATING NEW NODES WITH MATCHES FROM ABOVE

updateWithMatchesFromAbove :: Env -> a -> STM ()
updateWithMatchesFromAbove = undefined
