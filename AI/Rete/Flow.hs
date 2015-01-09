{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
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
import           Kask.Control.Monad (mapMM_, forMM_, toListM)

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
    Just oldSet -> Map.insert k newSet index
      where newSet = Set.delete wme oldSet
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

-- GENERATING IDS

-- | Generates a new Id.
genid :: Env -> STM Id
genid Env { envIdState = eid } = do
  recent <- readTVar eid

  -- Hopefully not in a reasonable time
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

-- | Interns and returns a Symbol represented by the String argument.
internSymbol :: Env -> String -> STM Symbol
internSymbol env name = case symbolName name of
  EmptyConst     -> return (Const emptyConstant)
  EmptyVar       -> return (Var   emptyVariable)
  OneCharConst   -> liftM Const (internConstant env name)
  MultiCharVar   -> liftM Var   (internVariable env name)
  MultiCharConst -> liftM Const (internConstant env name)

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

-- ALPHA MEMORY

-- | Activates the alpha memory by passing it a wme.
activateAmem :: Amem -> Wme -> STM ()
activateAmem amem wme = do
  addWmeToAmem amem wme
  addAmemToWme amem wme
  rightActivateAmemSuccessors amem wme

addWmeToAmem :: Amem -> Wme -> STM ()
addWmeToAmem amem wme = do
  modifyTVar' (amemWmes amem) (Set.insert wme)
{-# INLINE addWmeToAmem #-}

addAmemToWme :: Amem -> Wme -> STM ()
addAmemToWme amem Wme { wmeAmems = amems } = modifyTVar' amems (amem:)
{-# INLINE addAmemToWme #-}

rightActivateAmemSuccessors :: Amem -> Wme -> STM ()
rightActivateAmemSuccessors Amem { amemSuccessors = succs } wme =
  mapMM_ (rightActivateAmemSuccessor wme) (toListT succs)
{-# INLINE rightActivateAmemSuccessors #-}

rightActivateAmemSuccessor :: Wme -> AmemSuccessor -> STM ()
rightActivateAmemSuccessor = undefined
