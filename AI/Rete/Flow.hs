{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Flow
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-22
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
------------------------------------------------------------------------
module AI.Rete.Flow where

import           AI.Rete.Data
import           Control.Concurrent.STM
import           Control.Monad (when, unless, liftM, liftM2, forM_)
import           Data.Foldable (Foldable, toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable)
import           Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Sequence as Seq
import           Kask.Control.Monad (forMM_, toListM, whenM)
import           Kask.Data.Sequence (removeFirstOccurence,
                                     insertBeforeFirstOccurence)

-- MISC. UTILS

-- | A monadic (in STM monad) version of Set.null.
nullTSet :: TVar (Set.HashSet a) -> STM Bool
nullTSet = liftM Set.null . readTVar
{-# INLINE nullTSet #-}

-- | A monadic (in STM monad) version of Data.Foldable.toList.
toListT :: Foldable f => TVar (f a) -> STM [a] -- TSeq a -> STM [a]
toListT = toListM . readTVar
{-# INLINE toListT #-}

toTSeqFront :: TVar (Seq.Seq a) -> a -> STM ()
toTSeqFront s = modifyTVar' s . (Seq.<|)
{-# INLINE toTSeqFront #-}

toTSeqEnd :: TVar (Seq.Seq a) -> a -> STM ()
toTSeqEnd s x = modifyTVar' s (Seq.|> x)
{-# INLINE toTSeqEnd #-}

-- | Casting to Bool values
class ToBool a where toBool :: a -> Bool

-- WMES INDEXES MANIPULATION

type WmesIndexOperator a =
  (Hashable a, Eq a) => a -> Wme -> WmesIndex a -> WmesIndex a

-- | Creates an updated version of the wme index by putting a new
-- wme under the key k.
wmesIndexInsert ::  WmesIndexOperator a
wmesIndexInsert k wme index = Map.insert k newSet index
  where oldSet = Map.lookupDefault Set.empty k index
        newSet = Set.insert wme oldSet
{-# INLINE wmesIndexInsert #-}

-- | Removes the passed wme (possibly) stored under the key k from the
-- index.
wmesIndexDelete :: WmesIndexOperator a
wmesIndexDelete k wme index =
  case Map.lookup k index of
    Nothing     -> index
    Just oldSet -> Map.insert k (Set.delete wme oldSet) index
{-# INLINE wmesIndexDelete #-}

-- ENVIRONMENT

-- | Creates and returns a new, empty Env.
createEnv :: STM Env
createEnv = do
  -- Dummies.
  dttChildren    <- newTVar Set.empty
  dttNjResults   <- newTVar Set.empty
  dttNccResults  <- newTVar Set.empty
  dttOwner       <- newTVar Nothing

  theDtnJoins    <- newTVar Set.empty
  theDtnNegs     <- newTVar Set.empty
  theDtnNccs     <- newTVar Set.empty
  theDtnChildren <- newTVar Seq.empty

  let dtn = Dtn { dtnTok            = dtt
                , dtnAllJoins       = theDtnJoins
                , dtnAllNegs        = theDtnNegs
                , dtnAllNccs        = theDtnNccs
                , dtnChildren       = theDtnChildren }

      dtt = Tok { tokId             = -1
                , tokParent         = Nothing
                , tokWme            = Nothing
                , tokNode           = DtnTokNode dtn
                , tokChildren       = dttChildren
                , tokNegJoinResults = dttNjResults
                , tokNccResults     = dttNccResults
                , tokOwner          = dttOwner}

  -- Env components.
  idState      <- newTVar 0
  constants    <- newTVar Map.empty
  variables    <- newTVar Map.empty
  wmes         <- newTVar Map.empty
  wmesByObj    <- newTVar Map.empty
  wmesByAttr   <- newTVar Map.empty
  wmesByVal    <- newTVar Map.empty
  amems        <- newTVar Map.empty
  prods        <- newTVar Set.empty

  -- Let's create Env.
  return Env { envIdState    = idState
             , envConstants  = constants
             , envVariables  = variables
             , envWmes       = wmes
             , envWmesByObj  = wmesByObj
             , envWmesByAttr = wmesByAttr
             , envWmesByVal  = wmesByVal
             , envAmems      = amems
             , envProds      = prods
             , envDtn        = dtn }

feedEnvIndexes :: Env -> Wme -> STM ()
feedEnvIndexes
  Env     { envWmesByObj  = byObj
          , envWmesByAttr = byAttr
          , envWmesByVal  = byVal }
  wme@Wme { wmeObj        = obj
          , wmeAttr       = attr
          , wmeVal        = val } = do

    let w = Const wildcardConstant

    modifyTVar' byObj  (wmesIndexInsert obj      wme)
    modifyTVar' byObj  (wmesIndexInsert (Obj w)  wme)

    modifyTVar' byAttr (wmesIndexInsert attr     wme)
    modifyTVar' byAttr (wmesIndexInsert (Attr w) wme)

    modifyTVar' byVal  (wmesIndexInsert val      wme)
    modifyTVar' byVal  (wmesIndexInsert (Val w)  wme)
{-# INLINE feedEnvIndexes #-}

deleteFromEnvIndexes :: Env -> Wme -> STM ()
deleteFromEnvIndexes
  Env     { envWmesByObj  = byObj
          , envWmesByAttr = byAttr
          , envWmesByVal  = byVal}
  wme@Wme { wmeObj        = obj
          , wmeAttr       = attr
          , wmeVal        = val } = do

    let w = Const wildcardConstant

    modifyTVar' byObj  (wmesIndexDelete obj      wme)
    modifyTVar' byObj  (wmesIndexDelete (Obj w)  wme)

    modifyTVar' byAttr (wmesIndexDelete attr     wme)
    modifyTVar' byAttr (wmesIndexDelete (Attr w) wme)

    modifyTVar' byVal  (wmesIndexDelete val      wme)
    modifyTVar' byVal  (wmesIndexDelete (Val w)  wme)
{-# INLINE deleteFromEnvIndexes #-}

-- GENERATING IDS

-- | Generates a new Id.
genid :: Env -> STM Id
genid Env { envIdState = eid } = do
  recent <- readTVar eid

  -- Hopefully not in a achievable time, but ...
  when (recent == maxBound) (error "PANIC (1): Id OVERFLOW.")

  let new = recent + 1
  writeTVar eid new
  return new
{-# INLINE genid #-}

-- SPECIAL SYMBOLS

emptyConstant :: Constant
emptyConstant =  Constant (-1) ""

emptyVariable :: Variable
emptyVariable =  Variable (-2) "?"

wildcardConstant :: Constant
wildcardConstant = Constant (-3) "*"

-- INTERNING CONSTANTS AND VARIABLES

class Symbolic a where
  -- | Interns and returns a Symbol for the name argument.
  internSymbol :: Env -> a -> STM Symbol

  -- | Only if there is an interned symbol of the given name,
  -- returns Just it, Nothing otherwise.
  internedSymbol :: Env -> a -> STM (Maybe Symbol)

instance Symbolic Symbol where
  -- We may simply return the argument here, because Constants and
  -- Variables once interned never expire (get un-interned). Otherwise
  -- we would have to intern the argument's name.
  internSymbol   _ = return
  internedSymbol _ = return . Just
  {-# INLINE internSymbol   #-}
  {-# INLINE internedSymbol #-}

instance Symbolic String where
  internSymbol env name = case symbolName name of
    EmptyConst     -> return (Const emptyConstant)
    EmptyVar       -> return (Var   emptyVariable)
    OneCharConst   -> liftM  Const (internConstant env name)
    MultiCharVar   -> liftM  Var   (internVariable env name)
    MultiCharConst -> liftM  Const (internConstant env name)

  internedSymbol env name = case symbolName name of
    EmptyConst     -> return $! Just (Const emptyConstant)
    EmptyVar       -> return $! Just (Var   emptyVariable)
    OneCharConst   -> internedConstant env name
    MultiCharVar   -> internedVariable env name
    MultiCharConst -> internedConstant env name
  {-# INLINE internSymbol   #-}
  {-# INLINE internedSymbol #-}

data SymbolName = EmptyConst
                | EmptyVar
                | OneCharConst
                | MultiCharVar
                | MultiCharConst deriving Show

symbolName :: String -> SymbolName
symbolName "" = EmptyConst
symbolName [c]
  | c == '?'  = EmptyVar
  | otherwise = OneCharConst
symbolName (c:_:_)
  | c == '?'  = MultiCharVar
  | otherwise = MultiCharConst
{-# INLINE symbolName #-}

internConstant :: Env -> String -> STM Constant
internConstant env name = do
  cs <- readTVar (envConstants env)
  case Map.lookup name cs of
    Just c  -> return c
    Nothing -> do
      id' <- genid env
      let c = Constant id' name
      writeTVar (envConstants env) $! Map.insert name c cs
      return c
{-# INLINE internConstant #-}

internVariable :: Env -> String -> STM Variable
internVariable env name = do
  vs <- readTVar (envVariables env)
  case Map.lookup name vs of
    Just v  -> return v
    Nothing -> do
      id' <- genid env
      let v = Variable id' name
      writeTVar (envVariables env) $! Map.insert name v vs
      return v
{-# INLINE internVariable #-}

internedConstant :: Env -> String -> STM (Maybe Symbol)
internedConstant Env { envConstants = consts } name = do
  cs <- readTVar consts
  case Map.lookup name cs of
    Nothing -> return Nothing
    Just c  -> return $! Just (Const c)
{-# INLINE internedConstant #-}

internedVariable :: Env -> String -> STM (Maybe Symbol)
internedVariable Env { envVariables = vars } name = do
  vs <- readTVar vars
  case Map.lookup name vs of
    Nothing -> return Nothing
    Just v  -> return $! Just (Var v)
{-# INLINE internedVariable #-}

-- ALPHA MEMORY

-- | Activates the alpha memory by passing it a wme.
activateAmem :: Env -> Amem -> Wme -> STM ()
activateAmem env amem wme = do
  -- Add wme to amem's registry and indices.
  modifyTVar' (amemWmes       amem) (Set.insert                    wme)
  modifyTVar' (amemWmesByObj  amem) (wmesIndexInsert (wmeObj  wme) wme)
  modifyTVar' (amemWmesByAttr amem) (wmesIndexInsert (wmeAttr wme) wme)
  modifyTVar' (amemWmesByVal  amem) (wmesIndexInsert (wmeVal  wme) wme)

  -- Add amem to wme's amems.
  modifyTVar' (wmeAmems wme) (amem:)

  -- Activate amem successors.
  forMM_ (toListT (amemSuccessors amem)) $ \s ->
    case s of
      JoinAmemSuccessor join -> rightActivateJoin env join wme
      NegAmemSuccessor  neg  -> rightActivateNeg  env neg  wme

-- WMES

-- | Adds a new fact represented by three fields and returns its Wme.
-- When a Wme already exists in the system, does and returns Nothing.
addWme :: (Symbolic a, Symbolic b, Symbolic c) =>
          Env -> a -> b -> c -> STM (Maybe Wme)
addWme env obj attr val = do
  obj'  <- internSymbol env obj
  attr' <- internSymbol env attr
  val'  <- internSymbol env val

  let k = WmeKey (Obj obj') (Attr attr') (Val val')
  wmes <- readTVar (envWmes env)
  if Map.member k wmes
    then return Nothing -- Already present, do nothing.
    else do
      wme <- createWme env (Obj obj') (Attr attr') (Val val')

      -- Add wme to envWmes under k.
      writeTVar (envWmes env) $! Map.insert k wme wmes

      -- Add wme to env indexes (including wildcard key).
      feedEnvIndexes env wme

      -- Propagate wme into amems and return.
      feedAmems env wme (Obj obj') (Attr attr') (Val val')
      return (Just wme)

-- | Works like addWme inside an action (of a production).
addWmeA :: (Symbolic a, Symbolic b, Symbolic c) =>
           Actx -> a -> b -> c -> STM (Maybe Wme)
addWmeA actx = addWme (actxEnv actx)
{-# INLINE addWmeA #-}

-- | Creates an empty Wme.
createWme :: Env -> Obj -> Attr -> Val -> STM Wme
createWme env obj attr val = do
  id'       <- genid env
  amems     <- newTVar []
  toks      <- newTVar Set.empty
  njResults <- newTVar Set.empty

  return Wme { wmeId             = id'
             , wmeObj            = obj
             , wmeAttr           = attr
             , wmeVal            = val
             , wmeAmems          = amems
             , wmeToks           = toks
             , wmeNegJoinResults = njResults }
{-# INLINE createWme #-}

-- | Looks for an Amem corresponding with WmeKey k and activates
-- it. Does nothing unless finds one.
feedAmem :: Env -> Map.HashMap WmeKey Amem -> Wme -> WmeKey -> STM ()
feedAmem env amems wme k = case Map.lookup k amems of
  Just amem -> activateAmem env amem wme
  Nothing   -> return ()
{-# INLINE feedAmem #-}

-- | Feeds proper Amems with a Wme.
feedAmems :: Env -> Wme -> Obj -> Attr -> Val -> STM ()
feedAmems env wme o a v = do
  let w = Const wildcardConstant
  amems <- readTVar (envAmems env)

  feedAmem env amems wme $! WmeKey o       a        v
  feedAmem env amems wme $! WmeKey o       a        (Val w)
  feedAmem env amems wme $! WmeKey o       (Attr w) v
  feedAmem env amems wme $! WmeKey o       (Attr w) (Val w)

  feedAmem env amems wme $! WmeKey (Obj w) a        v
  feedAmem env amems wme $! WmeKey (Obj w) a        (Val w)
  feedAmem env amems wme $! WmeKey (Obj w) (Attr w) v
  feedAmem env amems wme $! WmeKey (Obj w) (Attr w) (Val w)
{-# INLINE feedAmems #-}

-- TOKS

-- | Creates a new Tok. It DOES NOT add it to the node (see
-- makeAndInsertTok).
makeTok :: Env -> Tok -> Maybe Wme -> TokNode -> STM Tok
makeTok env parentTok wme node = do
  id'        <- genid   env
  children   <- newTVar Set.empty
  njResults  <- newTVar Set.empty
  nccResults <- newTVar Set.empty
  owner      <- newTVar Nothing

  let tok = Tok { tokId             = id'
                , tokParent         = Just parentTok
                , tokWme            = wme
                , tokNode           = node
                , tokChildren       = children
                , tokNegJoinResults = njResults
                , tokNccResults     = nccResults
                , tokOwner          = owner}

  -- Add tok to parent.children (for tree-based removal) ...
  modifyTVar' (tokChildren parentTok) (Set.insert tok)

  -- Add tok to wme.tokens (for tree-based-removal) ...
  case wme of
    Just w  -> modifyTVar' (wmeToks w) (Set.insert tok)
    Nothing -> return () -- ... but only when wme is present.

  return tok
{-# INLINE makeTok #-}

-- | Creates a new Tok and adds it to the tokset (presumably in the node).
makeAndInsertTok :: Env -> Tok -> Maybe Wme
                 -> TokNode -> TVar TokSet -> STM Tok
makeAndInsertTok env parentTok wme node tokset = do
  tok <- makeTok env parentTok wme node
  modifyTVar' tokset (Set.insert tok)
  return tok
{-# INLINE makeAndInsertTok #-}

tokWmes :: Tok -> [Maybe Wme]
tokWmes = loop . Just
  where
    loop (Just tok) = tokWme tok : loop (tokParent tok)
    loop Nothing    = []
{-# INLINE tokWmes #-}

-- GENERALIZED NODES

leftActivateCondChild :: Env -> CondChild -> Tok -> Maybe Wme -> STM ()
leftActivateCondChild env child = case child of
  BmemCondChild    bmem    -> leftActivateBmem    env bmem
  NegCondChild     neg     -> leftActivateNeg     env neg
  NccCondChild     ncc     -> leftActivateNcc     env ncc
  PartnerCondChild partner -> leftActivatePartner env partner
  ProdCondChild    prod    -> leftActivateProd    env prod
{-# INLINE leftActivateCondChild #-}

leftActivateCondNode :: Env -> CondNode -> Tok -> Maybe Wme -> STM ()
leftActivateCondNode env child tok wme = case child of
  PosCondNode join -> leftActivateJoin env join tok
  NegCondNode neg  -> leftActivateNeg  env neg  tok wme
  NccCondNode ncc  -> leftActivateNcc  env ncc  tok wme
{-# INLINE leftActivateCondNode #-}

-- BMEM

-- | Performs left-activation of a Bmem.
leftActivateBmem :: Env -> Bmem -> Tok -> Maybe Wme -> STM ()
leftActivateBmem env bmem tok wme = do
  newTok <- makeAndInsertTok env tok wme (BmemTokNode bmem)
            (bmemToks bmem)

  forMM_ (toListT (bmemChildren bmem)) $ \join ->
    leftActivateJoin env join newTok
{-# INLINE leftActivateBmem #-}

-- UNINDEXED JOIN

-- | Performs the join tests not using any kind of indexing. Useful
-- while right-activation, when the Amem passes a single Wme, so
-- there is no use of Amem indexing.
performJoinTests :: [JoinTest] -> Tok -> Wme -> Bool
performJoinTests tests tok wme = all (passJoinTest (tokWmes tok) wme) tests
{-# INLINE performJoinTests #-}

passJoinTest :: [Maybe Wme] -> Wme -> JoinTest -> Bool
passJoinTest wmes wme
  JoinTest { joinField1 = f1, joinField2 = f2, joinDistance = d } =
    fieldSymbol f1 wme == fieldSymbol f2 wme2
    where
      wme2 = fromMaybe (error "PANIC (2): wmes !! d RETURNED Nothing.")
                       (wmes !! d)
{-# INLINE passJoinTest #-}

-- | Returns a value of a Field in Wme.
fieldSymbol :: Field -> Wme -> Symbol
fieldSymbol O Wme { wmeObj  = Obj  s } = s
fieldSymbol A Wme { wmeAttr = Attr s } = s
fieldSymbol V Wme { wmeVal  = Val  s } = s
{-# INLINE fieldSymbol #-}

-- INDEXED JOIN

-- | Matches a token to wmes in Amem using the Amem's indexes.
matchingAmemWmes :: [JoinTest] -> Tok -> Amem -> STM [Wme]
matchingAmemWmes [] _ amem = toListT (amemWmes amem) -- No tests, take all Wmes.
matchingAmemWmes tests tok amem = -- At least one test specified.
  toListM (foldr (liftM2 Set.intersection) s sets)
  where
    (s:sets) = map (amemWmesForTest (tokWmes tok) amem) tests
{-# INLINE matchingAmemWmes #-}

amemWmesForTest :: [Maybe Wme] -> Amem -> JoinTest -> STM WmeSet
amemWmesForTest wmes amem
  JoinTest { joinField1 = f1, joinField2 = f2, joinDistance = d } =
    case f1 of
      O -> amemWmesForIndex (Obj  value) (amemWmesByObj  amem)
      A -> amemWmesForIndex (Attr value) (amemWmesByAttr amem)
      V -> amemWmesForIndex (Val  value) (amemWmesByVal  amem)
    where
      wme   = fromMaybe (error "PANIC (3): wmes !! d RETURNED Nothing.")
                        (wmes !! d)
      value = fieldSymbol f2 wme
{-# INLINE amemWmesForTest #-}

amemWmesForIndex :: (Hashable a, Eq a) => a -> TVar (WmesIndex a) -> STM WmeSet
amemWmesForIndex k index =
  liftM (Map.lookupDefault Set.empty k) (readTVar index)
{-# INLINE amemWmesForIndex #-}

-- JOIN

leftActivateJoin :: Env -> Join -> Tok -> STM ()
leftActivateJoin env join tok = do
  let amem = joinAmem join
  isAmemEmpty <- nullTSet (amemWmes amem)

  -- When join.parent just became non-empty.
  whenM (isRightUnlinked (JoinAmemSuccessor join)) $ do
    relinkToAmem (JoinAmemSuccessor join)
    when isAmemEmpty $ leftUnlink join

  unless isAmemEmpty $ do
    children <- readTVar (joinChildren join)
    -- Only when we have children to activate ...
    unless (Seq.null children) $ do
      -- ... take matching Wmes from Amem indexes
      wmes <- matchingAmemWmes (joinTests join) tok amem
      -- and iterate all wmes over all children left-activating:
      forM_ wmes $ \wme -> forM_ (toList children) $ \child ->
        leftActivateCondChild env child tok (Just wme)

rightActivateJoin :: Env -> Join -> Wme -> STM ()
rightActivateJoin env join wme = do
  let parent = joinParent join

  parentTokSet <- joinParentTokSet parent
  let isParentEmpty = Set.null parentTokSet

  whenM (isLeftUnlinked join) $ do -- When join.amem just became non-empty.
    relinkToParent join parent
    when isParentEmpty (rightUnlink (JoinAmemSuccessor join))

  -- Only when parent has some Toks inside,
  unless isParentEmpty $ do
    children <- readTVar (joinChildren join)
    -- Only when there are some children...
    unless (Seq.null children) $
      -- Iterate over parent.toks ...
      forM_ (toList parentTokSet) $ \tok ->
        when (performJoinTests (joinTests join) tok wme) $
          -- ... and JoinChildren performing left activation.
          forM_ (toList children) $ \child ->
            leftActivateCondChild env child tok (Just wme)

joinParentTokSet :: JoinParent -> STM TokSet
joinParentTokSet parent = case parent of
  DtnJoinParent  Dtn  { dtnTok   = dtt  } -> return (Set.singleton dtt)
  BmemJoinParent Bmem { bmemToks = toks } -> readTVar toks
{-# INLINE joinParentTokSet #-}

-- NEG

leftActivateNeg :: Env -> Neg -> Tok -> Maybe Wme -> STM ()
leftActivateNeg env neg tok wme = do
  toks <- readTVar (negToks neg)
  whenM (isRightUnlinked (NegAmemSuccessor neg)) $
    -- The right-unlinking status above must be checked because a
    -- negative node is not right unlinked on creation.
    when (Set.null toks) $ relinkToAmem (NegAmemSuccessor neg)

  -- Build a new token and store it just like a Bmem would.
  newTok <- makeTok env tok wme (NegTokNode neg)
  writeTVar (negToks neg) (Set.insert newTok toks)

  let amem = negAmem neg
  isAmemEmpty <- nullTSet (amemWmes amem)

  -- Compute the join results (using amem indexes)
  unless isAmemEmpty $ do
    wmes <- matchingAmemWmes (negTests neg) newTok amem
    forM_ wmes $ \w -> do
      let jr = NegJoinResult newTok w
      modifyTVar' (tokNegJoinResults newTok) (Set.insert jr)
      modifyTVar' (wmeNegJoinResults w)      (Set.insert jr)
      -- In the original Doorenbos pseudo-code there was a bug - wme
      -- was used instead of w in the 3 lines above.

  -- If join results are empty, then inform children.
  whenM (nullTSet (tokNegJoinResults newTok)) $
    forMM_ (toListT (negChildren neg)) $ \child ->
      leftActivateCondChild env child newTok Nothing

rightActivateNeg :: Env -> Neg -> Wme -> STM ()
rightActivateNeg env neg wme =
  forMM_ (toListT (negToks neg)) $ \tok ->
    when (performJoinTests (negTests neg) tok wme) $ do
      whenM (nullTSet (tokNegJoinResults tok)) $
        deleteDescendentsOfTok env tok

      let jr = NegJoinResult tok wme
      -- Insert jr into tok.(neg)join-results.
      modifyTVar' (tokNegJoinResults tok) (Set.insert jr)
      -- Insert jr into wme.neg-join-results.
      modifyTVar' (wmeNegJoinResults wme) (Set.insert jr)

-- NCC

leftActivateNcc :: Env -> Ncc -> Tok -> Maybe Wme -> STM ()
leftActivateNcc env ncc tok wme = do
  let partner = nccPartner ncc

  -- Create a new token and add it to the nccToks index under a proper
  -- OwnerKey.
  newTok <- makeTok env tok wme (NccTokNode ncc)
  let ownerKey = OwnerKey tok wme
  modifyTVar' (nccToks ncc) (Map.insert ownerKey newTok)

  buff <- readTVar (partnerBuff partner)
  let isEmptyBuff = Set.null buff

  unless isEmptyBuff $ do
    -- Now we cut (clear) partner.buff and paste it into newTok.nccResults.
    writeTVar (partnerBuff   partner) Set.empty
    writeTVar (tokNccResults newTok)  buff
    -- For every result in buff result.owner = newTok.
    forM_ (toList buff) $ \result -> writeTVar (tokOwner result) $! Just newTok

  when isEmptyBuff $
    forMM_ (toListT (nccChildren ncc)) $ \child ->
      leftActivateCondChild env child newTok Nothing

-- (NCC) PARTNER

leftActivatePartner :: Env -> Partner -> Tok -> Maybe Wme -> STM ()
leftActivatePartner env partner tok wme = do
  let ncc = partnerNcc partner
  newResult <- makeTok env tok wme (PartnerTokNode partner)

  let n = partnerConjucts partner
      (ownerParent, ownerWme) = findOwnersPair n tok wme
  owner <- findNccOwner ncc ownerParent ownerWme

  case owner of
    Just owner' -> do
      -- Add newResult to owner's local memory and propagate further.
      modifyTVar' (tokNccResults owner') (Set.insert newResult)
      writeTVar   (tokOwner newResult) owner
      deleteDescendentsOfTok env owner'

    Nothing ->
      -- We didn't find an appropriate owner token already in the Ncc
      -- node's memory, so we just stuff the result in our temporary
      -- buffer.
      modifyTVar' (partnerBuff partner) (Set.insert newResult)

-- | To find the appropriate owner token (into whose local memory we
-- should put the result tok), we must first figure out what pair
-- (ownersParent, ownersWme) would represent the owner. To do this we
-- start with the argument pair and walk up the right number of links to
-- find the pair that emerged from the join node for the condition
-- preceding the Ncc partner.
-- [Taken from the original Doorenbos thesis (with small changes)]
findOwnersPair :: Int -> Tok -> Maybe Wme -> (Tok, Maybe Wme)
findOwnersPair 0 parentTok wme = (parentTok, wme)
findOwnersPair i parentTok _   = findOwnersPair (i-1) parentTok' wme
  where
    wme        = tokWme parentTok
    parentTok' = fromMaybe (error "PANIC (4): NO WAY TO ASK FOR PARENT.")
                 (tokParent parentTok)

-- | We search an owner (Tok) in ncc such that owner.parent =
-- parentTok and owner.wme = wme.
findNccOwner :: Ncc -> Tok -> Maybe Wme -> STM (Maybe Tok)
findNccOwner Ncc { nccToks = index } parentTok wme =
  liftM (Map.lookup (OwnerKey parentTok wme)) (readTVar index)
{-# INLINE findNccOwner #-}

-- PRODUCTION NODES

leftActivateProd :: Env -> Prod -> Tok -> Maybe Wme -> STM ()
leftActivateProd env prod tok wme = do
  newTok <- makeAndInsertTok env tok wme (ProdTokNode prod)
            (prodToks prod)
  -- Fire action
  let action = prodAction prod
  action (Actx env prod newTok (tokWmes newTok))

-- LEFT U/L (Joins ONLY)

instance ToBool LeftUnlinked  where
  toBool (LeftUnlinked  b) = b
  {-# INLINE toBool #-}

instance ToBool RightUnlinked where
  toBool (RightUnlinked b) = b
  {-# INLINE toBool #-}

isLeftUnlinked :: Join -> STM Bool
isLeftUnlinked join = liftM toBool (readTVar (joinLeftUnlinked join))
{-# INLINE isLeftUnlinked #-}

leftUnlink :: Join -> STM ()
leftUnlink join = do
  case joinParent join of
    BmemJoinParent bmem -> modifyTVar' (bmemChildren bmem) (Set.delete join)
    DtnJoinParent  dtn  -> modifyTVar' (dtnChildren dtn)
                           (removeFirstOccurence $! PosCondNode join)

  writeTVar (joinLeftUnlinked join) (LeftUnlinked True)
{-# INLINE leftUnlink #-}

relinkToParent :: Join -> JoinParent -> STM ()
relinkToParent join parent = do
  case parent of
    BmemJoinParent bmem -> modifyTVar' (bmemChildren bmem) (Set.insert join)
    DtnJoinParent  dtn  -> toTSeqFront (dtnChildren  dtn ) $! PosCondNode join

  writeTVar (joinLeftUnlinked join) (LeftUnlinked False)
{-# INLINE relinkToParent #-}

-- RIGHT U/L (Joins AND Negs - AmemSuccessors)

isRightUnlinked :: AmemSuccessor -> STM Bool
isRightUnlinked node = liftM toBool (readTVar (rightUnlinked node))
{-# INLINE isRightUnlinked #-}

rightUnlink :: AmemSuccessor -> STM ()
rightUnlink node = do
  modifyTVar' (amemSuccessors (nodeAmem node)) (removeFirstOccurence node)
  writeTVar   (rightUnlinked  node) (RightUnlinked True)
{-# INLINE rightUnlink #-}

relinkToAmem :: AmemSuccessor -> STM ()
relinkToAmem node = do
  let amem' = nodeAmem node
  ancestorLookup <- relinkAncestor node
  case ancestorLookup of
    Just ancestor ->
      -- insert node into node.amem.successors immediately before
      -- ancestor
      modifyTVar' (amemSuccessors amem')
        (node `insertBeforeFirstOccurence` ancestor)

    -- insert node at the tail of node.amem.successors
    Nothing -> toTSeqEnd (amemSuccessors amem') node

  writeTVar (rightUnlinked node) (RightUnlinked False)
{-# INLINE relinkToAmem #-}

-- | The goal of this loop is to find the nearest right linked
-- ancestor with the same Amem.
relinkAncestor :: AmemSuccessor -> STM (Maybe AmemSuccessor)
relinkAncestor node = do
  let possibleAncestor = nearestAncestor node
  case possibleAncestor of
    Nothing       -> return Nothing
    Just ancestor -> do
      rightUnlinked' <- isRightUnlinked ancestor
      if rightUnlinked'
        then relinkAncestor ancestor
        else return possibleAncestor

successorProp :: (Join -> a) -> (Neg -> a) -> AmemSuccessor -> a
successorProp f1 f2 node = case node of
  JoinAmemSuccessor join -> f1 join
  NegAmemSuccessor  neg  -> f2 neg
{-# INLINE successorProp #-}

rightUnlinked :: AmemSuccessor -> TVar RightUnlinked
rightUnlinked  = successorProp joinRightUnlinked negRightUnlinked
{-# INLINE rightUnlinked #-}

nearestAncestor :: AmemSuccessor -> Maybe AmemSuccessor
nearestAncestor = successorProp joinNearestAncestor negNearestAncestor
{-# INLINE nearestAncestor #-}

nodeAmem :: AmemSuccessor -> Amem
nodeAmem = successorProp joinAmem negAmem
{-# INLINE nodeAmem #-}

-- REMOVING WMES

-- | Removes the fact described by 3 symbols. Returns True on success
-- and False when the fact was not present in the system.
removeWme :: (Symbolic a, Symbolic b, Symbolic c) =>
             Env -> a -> b -> c -> STM Bool
removeWme env obj attr val = do
  obj'  <- internedSymbol env obj
  attr' <- internedSymbol env attr
  val'  <- internedSymbol env val

  if isJust obj' && isJust attr' && isJust val'
    then removeWmeImpl env
                       (Obj  (fromJust obj' ))
                       (Attr (fromJust attr'))
                       (Val  (fromJust val' ))

    -- At least 1 of the names didn't have a corresponding interned
    -- symbol, the wme can't exist.
    else return False

-- | Works like 'removeWme' inside an 'Action' (of a production).
removeWmeA :: (Symbolic a, Symbolic b, Symbolic c) =>
              Actx -> a -> b -> c -> STM Bool
removeWmeA actx = removeWme (actxEnv actx)
{-# INLINE removeWmeA #-}

removeWmeImpl :: Env -> Obj -> Attr -> Val -> STM Bool
removeWmeImpl env obj attr val = do
  wmes <- readTVar (envWmes env)
  let k = WmeKey obj attr val
  case Map.lookup k wmes of
    Nothing -> return False
    Just wme -> do
      -- Remove from Working Memory (Env registry and indexes)
      writeTVar (envWmes env) $! Map.delete k wmes
      deleteFromEnvIndexes env wme
      -- ... and propagate down the network.
      propagateWmeRemoval env wme obj attr val
      return True
{-# INLINE removeWmeImpl #-}

propagateWmeRemoval :: Env -> Wme -> Obj -> Attr -> Val -> STM ()
propagateWmeRemoval env wme obj attr val = do
  -- For every amem this wme belongs to ...
  forMM_ (readTVar (wmeAmems wme)) $ \amem -> do
    -- ... remove wme from amem indexes.
    modifyTVar' (amemWmesByObj  amem) (wmesIndexDelete obj  wme)
    modifyTVar' (amemWmesByAttr amem) (wmesIndexDelete attr wme)
    modifyTVar' (amemWmesByVal  amem) (wmesIndexDelete val  wme)
    wmes <- readTVar (amemWmes amem)
    let updatedWmes = Set.delete wme wmes
    writeTVar (amemWmes amem) updatedWmes

    when (Set.null updatedWmes) $ -- Amem just became empty, so ...
      -- ... left-unlink all Join successors.
      forMM_ (toListT (amemSuccessors amem)) $ \s ->
        case s of
          JoinAmemSuccessor join -> leftUnlink join
          NegAmemSuccessor  _    -> return ()

  -- Delete all tokens wme is in. We remove every token from it's
  -- parent token but avoid removing from wme.
  forMM_ (toListT (wmeToks wme)) $ \tok ->
    deleteTokAndDescendents env tok RemoveFromParent DontRemoveFromWme

  -- For every jr in wme.negative-join-results ...
  forMM_ (toListT (wmeNegJoinResults wme)) $ \jr -> do
    -- ... remove jr from jr.owner.negative-join-results.
    let owner = njrOwner jr
    jresults <- readTVar (tokNegJoinResults owner)
    let updatedJresults = Set.delete jr jresults
    writeTVar (tokNegJoinResults owner) updatedJresults

    -- If jr.owner.negative-join-results is nil
    when (Set.null updatedJresults) $
      leftActivateOwnerNodeChildren env owner
{-# INLINE propagateWmeRemoval #-}

leftActivateOwnerNodeChildren :: Env -> Tok -> STM ()
leftActivateOwnerNodeChildren env owner = case tokNode owner of
  DtnTokNode dtn -> forMM_ (toListT (dtnChildren dtn)) $ \child ->
    leftActivateCondNode env child owner Nothing

  NegTokNode neg -> forMM_ (toListT (negChildren neg)) $ \child ->
    leftActivateCondChild env child owner Nothing

  NccTokNode ncc -> forMM_ (toListT (nccChildren ncc)) $ \child ->
    leftActivateCondChild env child owner Nothing

  BmemTokNode bmem -> forMM_ (toListT (bmemChildren bmem)) $ \join ->
    leftActivateJoin env join owner

  PartnerTokNode _    -> error "PANIC (5): Partner CAN'T BE AN owner.node."
  ProdTokNode    _    -> error "PANIC (6): Prod CAN'T BE AN owner.node."
{-# INLINE leftActivateOwnerNodeChildren #-}

-- DELETING TOKS

data TokTokPolicy = RemoveFromParent | DontRemoveFromParent deriving Eq
data TokWmePolicy = RemoveFromWme    | DontRemoveFromWme    deriving Eq

-- | Deletes the descendents of the passed token.
deleteDescendentsOfTok :: Env -> Tok -> STM ()
deleteDescendentsOfTok env tok = do
  children <- readTVar (tokChildren tok)
  unless (Set.null children) $ do
    writeTVar (tokChildren tok) $! Set.empty
    -- Iteratively remove, skip removing from parent.
    forM_ (toList children) $ \child ->
      deleteTokAndDescendents env child DontRemoveFromParent RemoveFromWme

-- | Deletes the token and it's descendents.
deleteTokAndDescendents :: Env -> Tok -> TokTokPolicy -> TokWmePolicy -> STM ()
deleteTokAndDescendents env tok tokPolicy wmePolicy = do
  deleteDescendentsOfTok env tok
  removeTokFromItsNode   tok

  when (wmePolicy == RemoveFromWme) $ case tokWme tok of
    Nothing -> return ()
    Just w  -> modifyTVar' (wmeToks w) (Set.delete tok)

  when (tokPolicy == RemoveFromParent) $ case tokParent tok of
    Nothing -> error "PANIC (7): Dtt ??? Here ???!!!"
    Just t  -> modifyTVar' (tokChildren t) (Set.delete tok)

  -- Node-specific removal.
  case tokNode tok of
    DtnTokNode     _       -> error "PANIC (8): Dtt ??? Here ???!!!"
    BmemTokNode    bmem    -> nodeSpecificTokRemoval env bmem    tok
    NegTokNode     neg     -> nodeSpecificTokRemoval env neg     tok
    NccTokNode     ncc     -> nodeSpecificTokRemoval env ncc     tok
    PartnerTokNode partner -> nodeSpecificTokRemoval env partner tok
    ProdTokNode    prod    -> nodeSpecificTokRemoval env prod    tok

-- | If tok.node is not a Partner, remove it from tok.node.items.
removeTokFromItsNode :: Tok -> STM ()
removeTokFromItsNode tok = case tokNode tok of
  PartnerTokNode _    -> return ()
  DtnTokNode     _    -> error "PANIC (9): DO NOT REMOVE Dtt FROM Dtn."
  BmemTokNode    bmem -> removeTok        (bmemToks bmem) tok
  ProdTokNode    prod -> removeTok        (prodToks prod) tok
  NegTokNode     neg  -> removeTok        (negToks  neg)  tok
  NccTokNode     ncc  -> removeTokFromNcc ncc
  where
    removeTok var =  modifyTVar' var . Set.delete
    removeTokFromNcc Ncc { nccToks = toks } = modifyTVar' toks (Map.delete k)
      where
        t = fromMaybe (error "PANIC (10): Dtt ??? Here ???!!!") (tokParent tok)
        k = OwnerKey  t  (tokWme tok)
{-# INLINE removeTokFromItsNode #-}

class NodeSpecificTokRemoval a where
  nodeSpecificTokRemoval :: Env -> a -> Tok -> STM ()

instance NodeSpecificTokRemoval Bmem where
  nodeSpecificTokRemoval _ bmem _ =
    whenM (nullTSet (bmemToks bmem)) $ -- No items in here, so ...
      forMM_ (toListT (bmemChildren bmem)) $ \join ->
        -- ... let's right unlink children.
        rightUnlink (JoinAmemSuccessor join)
  {-# INLINE nodeSpecificTokRemoval #-}

instance NodeSpecificTokRemoval Neg  where
  nodeSpecificTokRemoval _ neg tok = do
    whenM (nullTSet (negToks neg)) $ rightUnlink (NegAmemSuccessor neg)

    -- For jr in tok.(neg)-join-results ...
    forMM_ (toListT (tokNegJoinResults tok)) $ \jr ->
      -- ... remove jr from jr.wme.negative-join-results.
      modifyTVar' (wmeNegJoinResults (njrWme jr)) (Set.delete jr)
  {-# INLINE nodeSpecificTokRemoval #-}

instance NodeSpecificTokRemoval Ncc where
  nodeSpecificTokRemoval _ _ tok =
    -- For result in tok.ncc-results ...
    forMM_ (toListT (tokNccResults tok)) $ \result -> do
      -- ... remove result from result.wme.tokens,
      case tokWme result of
        Nothing  -> error "PANIC (11): result.wme IS NIL (Nothing)."
        Just wme -> modifyTVar' (wmeToks wme) (Set.delete result)

      -- ... remove result from result.parent.children.
      case tokParent result of
        Nothing -> error "PANIC (12): Dtt ??? Here ???!!!"
        Just t  -> modifyTVar' (tokChildren t) (Set.delete result)
  {-# INLINE nodeSpecificTokRemoval #-}

instance NodeSpecificTokRemoval Partner where
  nodeSpecificTokRemoval env partner tok = do
    maybeOwner <- readTVar (tokOwner tok)
    case maybeOwner of
      Nothing    -> error "PANIC (13): tok.owner IS NIL (Nothing)."
      Just owner -> do
        -- Remove tok from tok.owner.ncc-results.
        nccResults <- readTVar (tokNccResults owner)
        let updatedNccResults = Set.delete tok nccResults
        writeTVar (tokNccResults owner) updatedNccResults

        -- If tok.owner.ncc-results is nil ...
        when (Set.null updatedNccResults) $
          -- .. left activate partner.ncc.children
          forMM_ (toListT (nccChildren (partnerNcc partner))) $ \child ->
            leftActivateCondChild env child owner Nothing
  {-# INLINE nodeSpecificTokRemoval #-}

instance NodeSpecificTokRemoval Prod where
  nodeSpecificTokRemoval env prod tok = case prodRevokeAction prod of
    -- Let's just fire a proper action (if present).
    Nothing     -> return ()
    Just action -> action (Actx env prod tok (tokWmes tok))
  {-# INLINE nodeSpecificTokRemoval #-}
