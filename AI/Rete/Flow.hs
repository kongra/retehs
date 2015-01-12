{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Control.Monad (when, liftM)
import           Data.Foldable (Foldable)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable)
import           Kask.Control.Monad (mapMM_, toListM)

-- MISC. UTILS

-- | A monadic (in STM monad) version of Set.null.
nullTSet :: TVar (Set.HashSet a) -> STM Bool
nullTSet = liftM Set.null . readTVar
{-# INLINE nullTSet #-}

-- | A monadic (in STM monad) version of Data.Foldable.toList.
toListT :: Foldable f => TVar (f a) -> STM [a] -- TSeq a -> STM [a]
toListT = toListM . readTVar
{-# INLINE toListT #-}

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
  idState    <- newTVar 0
  constants  <- newTVar Map.empty
  variables  <- newTVar Map.empty
  wmes       <- newTVar Map.empty
  wmesByObj  <- newTVar Map.empty
  wmesByAttr <- newTVar Map.empty
  wmesByVal  <- newTVar Map.empty
  amems      <- newTVar Map.empty
  prods      <- newTVar Set.empty

  return Env { envIdState    = idState
             , envConstants  = constants
             , envVariables  = variables
             , envWmes       = wmes
             , envWmesByObj  = wmesByObj
             , envWmesByAttr = wmesByAttr
             , envWmesByVal  = wmesByVal
             , envAmems      = amems
             , envProds      = prods }

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
  when (recent == maxBound) (error "Id overflow, can't go on.")

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

class InternSymbol a where
  -- | Interns and returns a Symbol represented by the argument.
  internSymbol :: Env -> a -> STM Symbol

instance InternSymbol Symbol where
  internSymbol _ = return

instance InternSymbol String where
  internSymbol env name = case symbolName name of
    EmptyConst     -> return (Const emptyConstant)
    EmptyVar       -> return (Var   emptyVariable)
    OneCharConst   -> liftM  Const (internConstant env name)
    MultiCharVar   -> liftM  Var   (internVariable env name)
    MultiCharConst -> liftM  Const (internConstant env name)

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

-- ACCESSING SYMBOLS (ALREADY INTERNED)

-- | Only if there is an interned symbol of the given name, returns
-- Just it, Nothing otherwise.
internedSymbol :: Env -> String -> STM (Maybe Symbol)
internedSymbol env name = case symbolName name of
    EmptyConst     -> return $! Just (Const emptyConstant)
    EmptyVar       -> return $! Just (Var   emptyVariable)
    OneCharConst   -> internedConstant env name
    MultiCharVar   -> internedVariable env name
    MultiCharConst -> internedConstant env name

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
  mapMM_ (rightActivateAmemSuccessor env wme) (toListT (amemSuccessors amem))

rightActivateAmemSuccessor :: Env -> Wme -> AmemSuccessor -> STM ()
rightActivateAmemSuccessor = undefined

-- WMES

class AddWme a where
  -- | Adds the fact represented by the Wme fields into the working
  -- memory and propagates the change downwards the Rete network. Returns
  -- the corresponding Wme. If the fact was already present, nothing
  -- happens, and Nothing is being returned.
  addWme :: Env -> a -> a -> a -> STM (Maybe Wme)

instance AddWme String where
  addWme env obj attr val = do
    obj'  <- internSymbol env obj
    attr' <- internSymbol env attr
    val'  <- internSymbol env val
    addWme env obj' attr' val'

instance AddWme Symbol where
  addWme env obj attr val = do
    let k = WmeKey (Obj obj) (Attr attr) (Val val)
    wmes <- readTVar (envWmes env)
    if Map.member k wmes
      then return Nothing -- Already present, do nothing.
      else do
        wme <- createWme env (Obj obj) (Attr attr) (Val val)

        -- Add wme to envWmes under k.
        writeTVar (envWmes env) $! Map.insert k wme wmes

        -- Add wme to env indexes (including wildcard key).
        feedEnvIndexes env wme

        -- Propagate wme into amems and return.
        feedAmems env wme (Obj obj) (Attr attr) (Val val)
        return (Just wme)

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
feedAmem :: Env -> Wme -> WmeKey -> STM ()
feedAmem env wme k = do
  amems <- readTVar (envAmems env)
  case Map.lookup k amems of
    Just amem -> activateAmem env amem wme
    Nothing   -> return ()
{-# INLINE feedAmem #-}

-- | Feeds proper Amems with a Wme.
feedAmems :: Env -> Wme -> Obj -> Attr -> Val -> STM ()
feedAmems env wme o a v = do
  let w = Const wildcardConstant

  feedAmem env wme $! WmeKey o       a        v
  feedAmem env wme $! WmeKey o       a        (Val w)
  feedAmem env wme $! WmeKey o       (Attr w) v
  feedAmem env wme $! WmeKey o       (Attr w) (Val w)

  feedAmem env wme $! WmeKey (Obj w) a        v
  feedAmem env wme $! WmeKey (Obj w) a        (Val w)
  feedAmem env wme $! WmeKey (Obj w) (Attr w) v
  feedAmem env wme $! WmeKey (Obj w) (Attr w) (Val w)
{-# INLINE feedAmems #-}
