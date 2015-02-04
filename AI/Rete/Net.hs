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
import           Control.Monad (forM_, liftM, unless)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Sequence as Seq
import           Kask.Control.Monad (whenM)
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
buildOrShareAmem env o a v = do
  let w  = Const wildcardConstant
      o' = Obj  $ if isVariable o then w else o
      a' = Attr $ if isVariable a then w else a
      v' = Val  $ if isVariable v then w else v

  amems <- readTVar (envAmems env)
  let k = WmeKey o' a' v'

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

      let amem = Amem { amemObj            = o'
                      , amemAttr           = a'
                      , amemVal            = v'
                      , amemSuccessors     = successors
                      , amemReferenceCount = refCount
                      , amemWmes           = wmes
                      , amemWmesByObj      = wmesByObj
                      , amemWmesByAttr     = wmesByAttr
                      , amemWmesByVal      = wmesByVal }

      -- Put amem into the env registry of Amems.
      writeTVar (envAmems env) $! Map.insert k amem amems

      activateAmemOnCreation env amem o' a' v'
      return amem

-- | A simplified, more effective version of amem activation that
-- takes place on the amem creation. No successors activation here,
-- cause no successors present.
activateAmemOnCreation :: Env -> Amem -> Obj -> Attr -> Val -> STM ()
activateAmemOnCreation env amem o a v = do
  byObjIndex  <- readTVar (envWmesByObj  env)
  byAttrIndex <- readTVar (envWmesByAttr env)
  byValIndex  <- readTVar (envWmesByVal  env)

  let wmesMatchingByObj  = Map.lookupDefault Set.empty o byObjIndex
      wmesMatchingByAttr = Map.lookupDefault Set.empty a byAttrIndex
      wmesMatchingByVal  = Map.lookupDefault Set.empty v byValIndex
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
      id'         <- genid   env
      children    <- newTVar Set.empty
      allChildren <- newTVar Map.empty
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

-- MISC. CONDS OPERATIONS

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
fieldEqualTo (PosCond (Obj o) (Attr a) (Val v)) s
  | o == s    = Just O
  | a == s    = Just A
  | v == s    = Just V
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
joinTestsForPosCond (i, PosCond o a v) = joinTestsForCondImpl i o a v
{-# INLINE joinTestsForPosCond #-}

joinTestsForNegCond :: IndexedNegCond -> [IndexedPosCond] -> [JoinTest]
joinTestsForNegCond (i, NegCond o a v) = joinTestsForCondImpl i o a v
{-# INLINE joinTestsForNegCond #-}

joinTestsForCondImpl :: Int -> Obj -> Attr -> Val
                     -> [IndexedPosCond] -> [JoinTest]
joinTestsForCondImpl i (Obj o) (Attr a) (Val v) earlierConds =
  result3
  where
    test1   = joinTestForField i o O earlierConds
    test2   = joinTestForField i a A earlierConds
    test3   = joinTestForField i v V earlierConds

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
  findAncestor (Left  join) amem = liftM JoinSuccessor (findAncestor join amem)
  findAncestor (Right neg ) amem = findAncestor neg amem

instance FindAncestor Bmem Join where
  findAncestor bmem = findAncestor (bmemParent bmem)

-- JOIN CREATION

buildOrShareDummyJoin :: Env -> Dtn -> Amem -> [JoinTest] -> STM Join
buildOrShareDummyJoin env parent amem tests = do
  -- Search for an existing dummy Join to share.
  allChildren <- readTVar (dtnAllChildren parent)
  let k = AmemSuccessorKey amem tests
  case Map.lookup k allChildren of
    Just join -> return join  -- Happily found.
    Nothing   -> do
      -- Let's build a new one.
      id'   <- genid   env
      bmem  <- newTVar Nothing
      negs  <- newTVar Map.empty
      prods <- newTVar Set.empty
      -- We consequently assume that there is no U/L of a dummy Join.
      lu    <- newTVar False
      ru    <- newTVar False

      let join = Join { joinId              = id'
                      , joinParent          = Left parent
                      , joinBmem            = bmem
                      , joinNegs            = negs
                      , joinProds           = prods
                      , joinAmem            = amem
                      , joinNearestAncestor = Nothing
                      , joinTests           = tests
                      , joinLeftUnlinked    = lu
                      , joinRightUnlinked   = ru }

      -- Add node to parent.allChildren.
      writeTVar (dtnAllChildren parent) $! Map.insert k join allChildren

      -- Add to front of amem.successors.
      modifyTVar' (amemSuccessors amem) (JoinSuccessor join Seq.<|)

      -- Increment amem.referenceCount.
      modifyTVar' (amemReferenceCount amem) (+1)

      return join

buildOrShareJoin :: Env -> Bmem -> Amem -> [JoinTest] -> STM Join
buildOrShareJoin env parent amem tests = do
  -- Search for an existing Join to share.
  allChildren <- readTVar (bmemAllChildren parent)
  let k = AmemSuccessorKey amem tests
  case Map.lookup k allChildren of
    Just join -> return join  -- Happily found.
    Nothing   -> do
      -- Let's build a new one.
      id'            <- genid    env
      bmem           <- newTVar  Nothing
      -- Establish the unlinking stuff ...
      unlinkRight    <- nullTSet (bmemToks parent)
      ru             <- newTVar  unlinkRight
      -- ... unlinking left only if the right ul. was not applied.
      unlinkLeft'    <- nullTSet (amemWmes amem)
      let unlinkLeft = not unlinkRight && unlinkLeft'
      lu             <- newTVar  unlinkLeft

      negs           <- newTVar  Map.empty -- Moved these lines from above
      prods          <- newTVar  Set.empty -- to cheat on hlint (duplication) :).

      let join = Join { joinId              = id'
                      , joinParent          = Right parent
                      , joinBmem            = bmem
                      , joinNegs            = negs
                      , joinProds           = prods
                      , joinAmem            = amem
                      , joinNearestAncestor = findAncestor parent amem
                      , joinTests           = tests
                      , joinLeftUnlinked    = lu
                      , joinRightUnlinked   = ru }

      -- Add node to parent.allChildren.
      writeTVar (bmemAllChildren parent) $! Map.insert k join allChildren

      -- Increment amem.referenceCount.
      modifyTVar' (amemReferenceCount amem) (+1)

      unless unlinkRight $
        -- Add to front of amem.successors.
        modifyTVar' (amemSuccessors amem) (JoinSuccessor join Seq.<|)

      unless unlinkLeft $
        -- Add to parent.children.
        modifyTVar' (bmemChildren parent) (Set.insert join)

      return join

-- NEG CREATION

buildOrShareNeg :: Env -> Either Join Neg -> Amem -> [JoinTest] -> STM Neg
buildOrShareNeg env parent amem tests = do
  let childrenVar = case parent of
        Left  join -> joinNegs join
        Right neg  -> negNegs  neg

  children <- readTVar childrenVar
  let k = AmemSuccessorKey amem tests
  case Map.lookup k children of
    Just neg -> return neg  -- Happily found.
    Nothing  -> do
      -- Let's build a new one.
      id'   <- genid   env
      negs  <- newTVar Map.empty
      prods <- newTVar Set.empty
      toks  <- newTVar Set.empty
      ru    <- newTVar False

      let neg = Neg { negId              = id'
                    , negParent          = parent
                    , negNegs            = negs
                    , negProds           = prods
                    , negToks            = toks
                    , negAmem            = amem
                    , negTests           = tests
                    , negNearestAncestor = findAncestor parent amem
                    , negRightUnlinked   = ru }

      -- Add neg to its parent.
      writeTVar childrenVar $! Map.insert k neg children

      -- Insert node at the head of amem.successors.
      modifyTVar' (amemSuccessors amem) (NegSuccessor neg Seq.<|)

      -- Increment amem.referenceCount.
      modifyTVar' (amemReferenceCount amem) (+1)

      updateFromAbove env neg parent

      -- Right unlink, but only after updating from above.
      whenM (nullTSet (negToks neg)) $ rightUnlink (NegSuccessor neg)

      return neg

-- UPDATING NEW NODES WITH MATCHES FROM ABOVE

class UpdateFromAbove a p where updateFromAbove :: Env -> a -> p -> STM ()

instance UpdateFromAbove Bmem Join where updateFromAbove = undefined

instance UpdateFromAbove Neg (Either Join Neg) where
  updateFromAbove = undefined

-- CREATING CONDITIONS

type FieldProducer a = Env -> STM a

-- | Positive condition.
data C = C !(FieldProducer Obj) !(FieldProducer Attr) !(FieldProducer Val)

-- | Negative (not) condition.
data N = N !(FieldProducer Obj) !(FieldProducer Attr) !(FieldProducer Val)

-- | Creates a positive condition.
c :: (Symbolic o, Symbolic a, Symbolic v) => o -> a -> v -> C
c o a v = C (toObj o) (toAttr a) (toVal v)
{-# INLINE c #-}

-- | Creates a negative (not) condition.
n :: (Symbolic o, Symbolic a, Symbolic v) => o -> a -> v -> N
n o a v = N (toObj o) (toAttr a) (toVal v)
{-# INLINE n #-}

-- ADDING PRODUCTIONS

class AddProduction e where
  -- | Adds a new production in the current context represented by e.
  addProduction  :: e -> C -> [C] -> [N] -> Action -> STM Prod

  -- -- | Works like addProduction but allows to define a revoke action.
  -- addProductionR :: e -> C -> [C] -> [N]
  --                -> Action  -- ^ Action
  --                -> Action  -- ^ Revoke Action
  --                -> STM Prod

-- CONFIGURING AND ACCESSING VARIABLE BINDINGS (IN ACTIONS)

variableBindingsForConds :: Int -> [IndexedPosCond] -> Bindings
variableBindingsForConds tokLen = loop Map.empty
  where
    loop result []                                           = result
    loop result ((i, PosCond (Obj o) (Attr a) (Val v)) : cs) =
      loop result3 cs
      where
        result1 = variableBindingsForCond o O d result
        result2 = variableBindingsForCond a A d result1
        result3 = variableBindingsForCond v V d result2
        d       = tokLen - i - 1
{-# INLINE variableBindingsForConds #-}

variableBindingsForCond :: Symbol -> Field -> Int -> Bindings -> Bindings
variableBindingsForCond s f d result = case s of
  -- For constants leave the resulting bindings untouched.
  Const _ -> result
  -- For vars avoid overriding existing bindings.
  Var   v -> if   Map.member v result    then result
             else Map.insert v (Location d f) result
{-# INLINE variableBindingsForCond #-}

-- | A value of a variable inside an action.
data VarVal = ValidVarVal   !Symbol
            | UnknownSymbol !String
            | ConstNotVar   !Constant
            | NoVarVal      !Variable

instance Show VarVal where
  show (ValidVarVal   s ) = show s
  show (UnknownSymbol s ) = "ERROR (1): UNKNOWN SYMBOL "   ++      s  ++ "."
  show (ConstNotVar   c') = "ERROR (2): CONST, NOT VAR "   ++ show c' ++ "."
  show (NoVarVal      v ) = "ERROR (3): NO VALUE FOR VAR " ++ show v  ++ "."
  {-# INLINE show #-}

-- | Returns a value of a variable inside an Action.
val :: Actx -> String -> STM VarVal
val Actx { actxEnv = env, actxProd = prod, actxWmes = wmes } s = do
  is <- internedSymbol env s
  case is of
    Nothing -> return (UnknownSymbol s)
    Just s' -> case s' of
      Const c' -> return (ConstNotVar c')
      Var   v  -> case Map.lookup v (prodBindings prod) of
        Nothing             -> return (NoVarVal v)
        Just (Location d f) ->
          return (ValidVarVal
                  (fieldSymbol f
                   (fromMaybe (error "PANIC (4): wmes !! d RETURNED Nothing.")
                              (wmes !! d))))

-- | Works like val, but raises an early error when a valid value
-- can't be returned.
valE :: Actx -> String -> STM Symbol
valE actx s = do
  result <- val actx s
  case result of { ValidVarVal v -> return v; _ -> error (show result) }

-- | Works like valE, but returns Nothing instead of raising an error.
valM :: Actx -> String -> STM (Maybe Symbol)
valM actx s = do
  result <- val actx s
  case result of { ValidVarVal v -> return (Just v); _ -> return Nothing }
