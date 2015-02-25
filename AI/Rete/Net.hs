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
module AI.Rete.Net
    (
      -- * Adding/removing productions
      addProd
    , addProdR
    , removeProd

      -- * Conditions
    , c
    , C
    , n
    , N
    , noMoreConds
    , noNegs

      -- * Accessing information in actions
    , val
    , valE
    , valM
    , VarVal (..)

      -- * Predefined Actions and Action-related utils
    , acompose
    , passAction
    , traceAction
    )
    where

import           AI.Rete.Data
import           AI.Rete.Flow
import           Control.Concurrent.STM
import           Control.Monad (forM_, liftM, liftM3, unless, when)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Sequence as Seq
import           Debug.Trace
import           Kask.Control.Monad (whenM, forMM_, unlessM)
import           Kask.Data.List (nthDef)
import           Kask.Data.Sequence (removeFirstOccurence)
import           Safe (headMay)

-- AMEM CREATION

-- | Searches for an existing alpha memory for the given symbols or
-- creates a new one.
buildOrShareAmem :: Env
                 -> Obj  ConstantOrVariable
                 -> Attr ConstantOrVariable
                 -> Val  ConstantOrVariable
                 -> STM  Amem
buildOrShareAmem env (Obj o) (Attr a) (Val v) = do
  let f s = case s of { JustConstant c' -> c'; _ -> wildcardConstant }
      o'  = Obj  (f o)
      a'  = Attr (f a)
      v'  = Val  (f v)

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

      let amem = Amem { amemObj        = o'
                      , amemAttr       = a'
                      , amemVal        = v'
                      , amemSuccessors = successors
                      , amemRefCount   = refCount
                      , amemWmes       = wmes
                      , amemWmesByObj  = wmesByObj
                      , amemWmesByAttr = wmesByAttr
                      , amemWmesByVal  = wmesByVal }

      -- Put amem into the env registry of Amems.
      writeTVar (envAmems env) $! Map.insert k amem amems

      activateAmemOnCreation env amem o' a' v'
      return amem

-- | A simplified, more effective version of amem activation that
-- takes place on the amem creation. No successors activation here,
-- cause no successors present.
activateAmemOnCreation :: Env
                       -> Amem
                       -> Obj  Constant
                       -> Attr Constant
                       -> Val  Constant
                       -> STM ()
activateAmemOnCreation env amem o a v = do
  let (Obj  o') = o
      (Attr a') = a
      (Val  v') = v
  wmes <- wmesForAmemFeed (isWildcard o') (isWildcard a') (isWildcard v')
          env o a v

  -- Put all matching wmes into the amem.
  writeTVar (amemWmes amem) wmes

  -- Iteratively work on every wme.
  forM_ (toList wmes) $ \wme -> do
    -- Put amem to wme registry of Amems.
    modifyTVar' (wmeAmems wme) (amem:)

    -- Put Wme into amem indexes
    modifyTVar' (amemWmesByObj  amem) (wmesIndexInsert (wmeObj  wme) wme)
    modifyTVar' (amemWmesByAttr amem) (wmesIndexInsert (wmeAttr wme) wme)
    modifyTVar' (amemWmesByVal  amem) (wmesIndexInsert (wmeVal  wme) wme)
  where
    isWildcard s = s == wildcardConstant
{-# INLINE activateAmemOnCreation #-}

wmesForAmemFeed :: Bool -> Bool -> Bool
                -> Env
                -> Obj Constant -> Attr Constant -> Val Constant
                -> STM (Set.HashSet Wme)
wmesForAmemFeed False False False env o a v = do
  -- o a v
  byObj  <- readTVar (envWmesByObj  env)
  byAttr <- readTVar (envWmesByAttr env)
  byVal  <- readTVar (envWmesByVal  env)
  let s1 = Map.lookupDefault Set.empty o byObj
      s2 = Map.lookupDefault Set.empty a byAttr
      s3 = Map.lookupDefault Set.empty v byVal
  return $ s1 `Set.intersection` s2 `Set.intersection` s3

wmesForAmemFeed False False True env o a _ = do
  -- o a w
  byObj  <- readTVar (envWmesByObj  env)
  byAttr <- readTVar (envWmesByAttr env)
  let s1 = Map.lookupDefault Set.empty o byObj
      s2 = Map.lookupDefault Set.empty a byAttr
  return $ s1 `Set.intersection` s2

wmesForAmemFeed False True False env o _ v = do
  -- o w v
  byObj  <- readTVar (envWmesByObj  env)
  byVal  <- readTVar (envWmesByVal  env)
  let s1 = Map.lookupDefault Set.empty o byObj
      s2 = Map.lookupDefault Set.empty v byVal
  return $ s1 `Set.intersection` s2

wmesForAmemFeed False True True env o _ _ =
  -- o w w
  liftM (Map.lookupDefault Set.empty o) (readTVar (envWmesByObj env))

wmesForAmemFeed True False False env _ a v = do
  -- w a v
  byAttr <- readTVar (envWmesByAttr env)
  byVal  <- readTVar (envWmesByVal  env)
  let s1 = Map.lookupDefault Set.empty a byAttr
      s2 = Map.lookupDefault Set.empty v byVal
  return $ s1 `Set.intersection` s2

wmesForAmemFeed True False True env _ a _ =
  -- w a w
  liftM (Map.lookupDefault Set.empty a) (readTVar (envWmesByAttr env))

wmesForAmemFeed True True False env _ _ v =
  -- w w v
  liftM (Map.lookupDefault Set.empty v) (readTVar (envWmesByVal env))

wmesForAmemFeed True True True env _ _ _ =
  -- w w w
  liftM (Set.fromList . Map.elems) (readTVar (envWmes env))
{-# INLINE wmesForAmemFeed #-}

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
-- Constant.
fieldEqualTo :: PosCond -> ConstantOrVariable -> Maybe Field
fieldEqualTo (PosCond (Obj o) (Attr a) (Val v)) s
  | o == s    = Just O
  | a == s    = Just A
  | v == s    = Just V
  | otherwise = Nothing
{-# INLINE fieldEqualTo #-}

matchingLocation :: ConstantOrVariable -> IndexedPosCond -> Maybe Location
matchingLocation s (i, cond) = case fieldEqualTo cond s of
  Nothing -> Nothing
  Just f  -> Just (Location i f)
{-# INLINE matchingLocation #-}

joinTestForField :: Int -> ConstantOrVariable -> Field -> [IndexedPosCond]
                 -> Maybe JoinTest
joinTestForField i v field earlierConds =
  case v of
    JustVariable _ -> case headMay (matches earlierConds) of
      Nothing              -> Nothing
      Just (Location i' f) -> Just (JoinTest field f (i - i' - 1))

    JustConstant _ -> Nothing -- No tests from Consts (non-Vars).
  where
    matches = map fromJust . filter isJust . map (matchingLocation v)
{-# INLINE joinTestForField #-}

joinTestsForPosCond :: IndexedPosCond -> [IndexedPosCond] -> [JoinTest]
joinTestsForPosCond (i, PosCond o a v) = joinTestsForCondImpl i o a v
{-# INLINE joinTestsForPosCond #-}

joinTestsForNegCond :: IndexedNegCond -> [IndexedPosCond] -> [JoinTest]
joinTestsForNegCond (i, NegCond o a v) = joinTestsForCondImpl i o a v
{-# INLINE joinTestsForNegCond #-}

joinTestsForCondImpl :: Int
                     -> Obj  ConstantOrVariable
                     -> Attr ConstantOrVariable
                     -> Val  ConstantOrVariable
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

buildOrShareDummyJoin :: Env -> Amem -> STM Join
buildOrShareDummyJoin env amem = do
  let parent = envDtn env
      tests  = []
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
      modifyTVar' (amemRefCount amem) (+1)

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
      modifyTVar' (amemRefCount amem) (+1)

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
      modifyTVar' (amemRefCount amem) (+1)

      updateFromAbove env neg parent

      -- Right unlink, but only after updating from above.
      whenM (nullTSet (negToks neg)) $ rightUnlink (NegSuccessor neg)

      return neg

-- CREATING CONDITIONS

type FieldMkr a = Env -> STM a

-- | Positive condition.
data C = C !(FieldMkr (Obj  ConstantOrVariable))
           !(FieldMkr (Attr ConstantOrVariable))
           !(FieldMkr (Val  ConstantOrVariable))

-- | Negative (not) condition.
data N = N !(FieldMkr (Obj  ConstantOrVariable))
           !(FieldMkr (Attr ConstantOrVariable))
           !(FieldMkr (Val  ConstantOrVariable))

toField :: ToConstantOrVariable a
        => (ConstantOrVariable -> b) -> a -> Env -> STM b
toField f v env = liftM f (toConstantOrVariable env v)

-- | Creates a positive condition.
c :: (ToConstantOrVariable o, ToConstantOrVariable a, ToConstantOrVariable v)
  => o -> a -> v -> C
c o a v = C (toField Obj o) (toField Attr a) (toField Val v)
{-# INLINE c #-}

-- | Creates a negative (not) condition.
n :: (ToConstantOrVariable o, ToConstantOrVariable a, ToConstantOrVariable v)
  => o -> a -> v -> N
n o a v = N (toField Obj o) (toField Attr a) (toField Val v)
{-# INLINE n #-}

toPosCond :: Env -> C -> STM PosCond
toPosCond env (C o a v) = liftM3 PosCond (o env) (a env) (v env)

toNegCond :: Env -> N -> STM NegCond
toNegCond env (N o a v) = liftM3 NegCond (o env) (a env) (v env)

-- ADDING PRODUCTIONS

class AddProd e where
  -- | Adds a new production in the current context represented by e.
  addProd :: e -> C -> [C] -> [N] -> Action -> STM Prod

  -- | Works like addProd but allows to define a revoke action.
  addProdR :: e -> C -> [C] -> [N] -> Action
           -> Action  -- ^ Revoke Action
           -> STM Prod

instance AddProd Env where
  addProd  env c' cs ns action        = addProdImpl env c' cs ns action Nothing
  addProdR env c' cs ns action revoke = addProdImpl env c' cs ns action
                                        (Just revoke)
  {-# INLINE addProd  #-}
  {-# INLINE addProdR #-}

instance AddProd Actx where
  addProd  Actx { actxEnv = env } = addProd  env
  addProdR Actx { actxEnv = env } = addProdR env
  {-# INLINE addProd  #-}
  {-# INLINE addProdR #-}

addProdImpl :: Env -> C -> [C] -> [N] -> Action -> Maybe Action -> STM Prod
addProdImpl env c' cs ns action revoke = do
  -- Prepare conditions.
  posConds    <- mapM (toPosCond env) (c':cs)
  negConds    <- mapM (toNegCond env) ns
  let ics = indexedPosConds                   posConds
      ins = indexedNegConds (length posConds) negConds

  -- Build or share dummy (top-level) Join for first PosCond.
  let PosCond o a v = head posConds
  dummyAmem <- buildOrShareAmem      env o a v
  dummyJoin <- buildOrShareDummyJoin env dummyAmem

  -- Build or share joins for ics tail.
  let ic1:ics' = ics
  bottomJoin <- buildOrShareJoins env ics' dummyJoin [ic1]

  -- Build or share negs for ins.
  parent <- buildOrShareNegs env ins (Left bottomJoin) (reverse ics)

  -- Prepare bindings.
  let tokLen   = length ics + length ins
      bindings = variableBindingsForConds tokLen (reverse ics)

  -- Create the Prod.
  id'  <- genid   env
  toks <- newTVar Set.empty
  let prod = Prod { prodId           = id'
                  , prodParent       = parent
                  , prodToks         = toks
                  , prodAction       = action
                  , prodRevokeAction = revoke
                  , prodBindings     = bindings }

  -- Add node to parent's children.
  let childrenVar = case parent of
        { Left join -> joinProds join; Right neg -> negProds neg }
  modifyTVar' childrenVar (Set.insert prod)

  -- Add the node to the registry in the env.
  modifyTVar' (envProds env) (Set.insert prod)

  -- Update with matches from above.
  updateFromAbove env prod parent

  return prod

buildOrShareJoins :: Env -> [IndexedPosCond] -> Join -> [IndexedPosCond]
                  -> STM Join
buildOrShareJoins _   []       lastJoin   _            = return lastJoin
buildOrShareJoins env (ic:ics) higherJoin earlierConds = do
  let (_, PosCond o a v) = ic
      tests              = joinTestsForPosCond ic earlierConds
  amem   <- buildOrShareAmem env o a v
  parent <- buildOrShareBmem env higherJoin
  join   <- buildOrShareJoin env parent amem tests

  buildOrShareJoins env ics join (ic : earlierConds)

buildOrShareNegs :: Env -> [IndexedNegCond] -> Either Join Neg -> [IndexedPosCond]
                 -> STM (Either Join Neg)
buildOrShareNegs _   []         lastNode _            = return lastNode
buildOrShareNegs env (ineg:ins) parent   earlierConds = do
  let (_, NegCond o a v) = ineg
      tests = joinTestsForNegCond ineg earlierConds
  amem <- buildOrShareAmem env o a v
  neg  <- buildOrShareNeg  env parent amem tests

  buildOrShareNegs env ins (Right neg) earlierConds

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

variableBindingsForCond :: ConstantOrVariable -> Field -> Int -> Bindings
                        -> Bindings
variableBindingsForCond s f d result = case s of
  -- For constants leave the resulting bindings untouched.
  JustConstant _ -> result
  -- For vars avoid overriding existing bindings.
  JustVariable v -> if   Map.member v result    then result
                    else Map.insert v (Location d f) result
{-# INLINE variableBindingsForCond #-}

-- | A value of a variable inside an action.
data VarVal = ValidVarVal   !Constant
            | NoVarVal      !Variable

instance Show VarVal where
  show (ValidVarVal   s ) = show s
  show (NoVarVal      v ) = "ERROR (1): NO VALUE FOR VAR " ++ show v  ++ "."
  {-# INLINE show #-}

-- | Returns a value of a variable inside an Action.
val :: Actx -> Var -> STM VarVal
val Actx { actxEnv = env, actxProd = prod, actxWmes = wmes } vi = do
  v <- vi env
  case Map.lookup v (prodBindings prod) of
    Nothing             -> return (NoVarVal v)
    Just (Location d f) -> return (ValidVarVal (fieldConstant f wme'))
      where
        wme  = nthDef    (error ("PANIC (6): ILLEGAL INDEX " ++ show d)) d wmes
        wme' = fromMaybe (error  "PANIC (7): wmes !! d RETURNED Nothing.") wme

-- | Works like val, but raises an early error when a valid value
-- can't be returned.
valE :: Actx -> Var -> STM Constant
valE actx s = do
  result <- val actx s
  case result of { ValidVarVal c' -> return c'; _ -> error (show result) }
{-# INLINE valE #-}

-- | Works like valE, but returns Nothing instead of raising an error.
valM :: Actx -> Var -> STM (Maybe Constant)
valM actx s = do
  result <- val actx s
  case result of { ValidVarVal c' -> return (Just c'); _ -> return Nothing }
{-# INLINE valM #-}

-- UPDATING NEW NODES WITH MATCHES FROM ABOVE

class UpdateFromAbove a p where updateFromAbove :: Env -> a -> p -> STM ()

instance UpdateFromAbove Bmem Join where
  updateFromAbove env bmem parent = updateFromJoin env parent
    (writeTVar (joinBmem parent) (Just bmem))
  {-# INLINE updateFromAbove #-}

instance UpdateFromAbove Neg Join where
  updateFromAbove env neg parent = updateFromJoin env parent
    (writeTVar (joinNegs parent) (Map.singleton k neg))
    where k = AmemSuccessorKey (negAmem neg) (negTests neg)
  {-# INLINE updateFromAbove #-}

instance UpdateFromAbove Prod Join where
  updateFromAbove env prod parent = updateFromJoin env parent
    (writeTVar (joinProds parent) (Set.singleton prod))
  {-# INLINE updateFromAbove #-}

instance UpdateFromAbove Neg Neg where
  updateFromAbove env neg parent = updateFromNeg parent $ \tok ->
    leftActivateNeg env neg (Right tok) Nothing
  {-# INLINE updateFromAbove #-}

instance UpdateFromAbove Prod Neg where
  updateFromAbove env prod parent = updateFromNeg parent $ \tok ->
    leftActivateProd env prod (Right tok) Nothing
  {-# INLINE updateFromAbove #-}

instance UpdateFromAbove Neg (Either Join Neg) where
  updateFromAbove env neg (Left  parent) = updateFromAbove env neg parent
  updateFromAbove env neg (Right parent) = updateFromAbove env neg parent
  {-# INLINE updateFromAbove #-}

instance UpdateFromAbove Prod (Either Join Neg) where
  updateFromAbove env prod (Left  parent) = updateFromAbove env prod parent
  updateFromAbove env prod (Right parent) = updateFromAbove env prod parent
  {-# INLINE updateFromAbove #-}

updateFromJoin :: Env -> Join -> STM () -> STM ()
updateFromJoin env parent setChild = do
  -- Save children.
  bmem <-  readTVar (joinBmem  parent)
  negs <-  readTVar (joinNegs  parent)
  prods <- readTVar (joinProds parent)

  -- Clear children.
  writeTVar (joinBmem  parent) Nothing
  writeTVar (joinNegs  parent) Map.empty
  writeTVar (joinProds parent) Set.empty

  -- Set singleton child.
  setChild

  -- Right activate parent.
  forMM_ (toListT (amemWmes (joinAmem parent))) $ \wme ->
    rightActivateJoin env parent wme

  -- Restore saved children.
  writeTVar (joinBmem  parent) bmem
  writeTVar (joinNegs  parent) negs
  writeTVar (joinProds parent) prods

updateFromNeg :: Neg -> (Ntok -> STM()) -> STM ()
updateFromNeg parent f = forMM_ (toListT (negToks parent)) $ \tok ->
  whenM (nullTSet (ntokNegJoinResults tok)) $ f tok
{-# INLINE updateFromNeg #-}

-- DELETING NODES

class DeleteNode a where
  -- | Deletes the node and any unused ancestor.
  deleteNode :: Env -> a -> STM ()

instance DeleteNode Bmem where
  deleteNode env bmem = do
    -- Delete all tokens. There is no need to unbind toks from this
    -- node, it will be deleted anyway.
    forMM_ (toListT (bmemToks bmem)) $ \btok ->
      deleteTokAndDescendents env (BmemWmeTok btok)
        RemoveFromParent RemoveFromWme DontRemoveFromNode

    -- Unbind node from its parent.
    let parent = bmemParent bmem
    writeTVar (joinBmem parent) Nothing

    -- If parent has no children, delete it too.
    children <- joinChildren parent
    when (nullJoinChildren children) $ deleteNode env parent

instance DeleteNode Join where
  deleteNode env join = do
    let amem = joinAmem join
    -- If join is NOT rightUnlinked, remove it from amem.successors.
    unlessM (readTVar (joinRightUnlinked join)) $
      modifyTVar' (amemSuccessors amem)
                  (removeFirstOccurence (JoinSuccessor join))

    -- If amem.referenceCount is 1 (this join), delete amem
    -- else decrement amem.referenceCount.
    rcount <- readTVar (amemRefCount amem)
    if rcount == 1
      then deleteAmem env amem
      else writeTVar (amemRefCount amem) $! rcount - 1

    -- If join is NOT leftUnlinked, remove it from its parent ...
    let parent = joinParent join
    unlessM (readTVar (joinLeftUnlinked join)) $ case parent of
      Right bmem -> modifyTVar' (bmemChildren bmem) (Set.delete join)
      Left  _    -> return ()  -- ... but not from Dtn (has no children).

    -- Remove join from parent.allChildren.
    -- If parent.allChildren is nil, delete parent (not Dtn).
    let allChildrenVar = case parent of
                           Right bmem -> bmemAllChildren bmem
                           Left  dtn  -> dtnAllChildren  dtn
    allChildren <- readTVar allChildrenVar
    let allChildren' = Map.delete k allChildren
        k            = AmemSuccessorKey amem (joinTests join)
    if Map.null allChildren'
      then (case parent of
              Right bmem -> deleteNode env bmem
              Left  _    -> return ())  -- Dtn must be kept.
      else writeTVar allChildrenVar allChildren'

instance DeleteNode Neg where
  deleteNode env neg = do
    -- Delete all tokens. There is no need to unbind toks from this
    -- node, it will be deleted anyway.
    forMM_ (toListT (negToks neg)) $ \ntok ->
      deleteTokAndDescendents env (NegWmeTok ntok)
        RemoveFromParent RemoveFromWme DontRemoveFromNode

    let amem = negAmem neg
    -- If neg is NOT rightUnlinked, remove it from amem.successors.
    unlessM (readTVar (negRightUnlinked neg)) $
      modifyTVar' (amemSuccessors amem)
                  (removeFirstOccurence (NegSuccessor neg))

    -- If amem.referenceCount is 1 (this neg), delete amem
    -- else decrement amem.referenceCount
    rcount <- readTVar (amemRefCount amem)
    if rcount == 1
      then deleteAmem env amem
      else writeTVar (amemRefCount amem) $! rcount - 1

    -- Unbind neg from its parent.
    let parent = negParent neg
        k      = AmemSuccessorKey amem (negTests neg)
    case parent of
      Left  join -> modifyTVar' (joinNegs join) (Map.delete k)
      Right neg' -> modifyTVar' (negNegs  neg') (Map.delete k)

    -- If parent has no children, delete it too.
    case parent of
      Left join -> do
        children <- joinChildren join
        when (nullJoinChildren children) $ deleteNode env join

      Right neg' -> do
        children <- negChildren neg'
        when (nullNegChildren children) $ deleteNode env neg'

instance DeleteNode Prod where
  deleteNode env prod = do
    -- Delete all tokens. There is no need to unbind toks from this
    -- node, it will be deleted anyway.
    forMM_ (toListT (prodToks prod)) $ \ptok ->
      deleteTokAndDescendents env (ProdWmeTok ptok)
        RemoveFromParent RemoveFromWme DontRemoveFromNode

    -- Unbind prod from its parent.
    case prodParent prod of
      Left  join -> modifyTVar' (joinProds join) (Set.delete prod)
      Right neg  -> modifyTVar' (negProds  neg ) (Set.delete prod)

    -- If prod.parent has no children, delete it too.
    case prodParent prod of
      Left join -> do
        children <- joinChildren join
        when (nullJoinChildren children) $ deleteNode env join

      Right neg -> do
        children <- negChildren neg
        when (nullNegChildren children) $ deleteNode env neg

deleteAmem :: Env -> Amem -> STM ()
deleteAmem
  Env  { envAmems = amems }
  Amem { amemObj  = o
       , amemAttr = a
       , amemVal  = v     } = modifyTVar' amems (Map.delete (WmeKey o a v))
{-# INLINE deleteAmem #-}

-- REMOVING PRODUCTIONS

class RemoveProd e where
  -- | Removes a production represented by the node. Returns True
  -- unless the production was already removed.
  removeProd :: e -> Prod -> STM Bool

instance RemoveProd Env where
  removeProd env prod = do
    prods <- readTVar (envProds env)
    if Set.member prod prods then
      (do  writeTVar (envProds env) $! Set.delete prod prods
           deleteNode env prod
           return True)

      else return False

instance RemoveProd Actx where
  removeProd Actx { actxEnv = env } = removeProd env
  {-# INLINE removeProd #-}

-- SOME PRODUCTION CREATION CONVENIENCES

-- | Symbolic representation of an empty list of C(onds).
noMoreConds :: [C]
noMoreConds = []

-- | Symbolic representation of an empty list of N(egative conds).
noNegs :: [N]
noNegs = []

-- PREDEFINED ACTIONS AND ACTION-RELATED OPS

-- | Composes the passed Actions.
acompose :: [Action] -> Action
acompose as actx = forM_ as $ \a -> a actx
{-# INLINE acompose #-}

-- | An action that doesn't do anything.
passAction :: Action
passAction _ = return ()

-- | An action that traces a predefined text on execution.
traceAction :: String -> Action
traceAction s _ = traceM s
