{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
import           Control.Monad (when)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- | Creates a new, empty Env. The procedure is not tagged in any way
-- because it belongs to the API and there is no explicit dependency
-- on it.
createEnv :: STM Env
createEnv = do
  idState         <- newTVar $! EnvIdState         0
  symbolsRegistry <- newTVar $! EnvSymbolsRegistry Map.empty
  varsRegistry    <- newTVar $! EnvVarsRegistry    Map.empty
  wmesRegistry    <- newTVar $! EnvWmesRegistry    Map.empty
  wmesByObj       <- newTVar $! EnvWmesByObj       Map.empty
  wmesByAttr      <- newTVar $! EnvWmesByAttr      Map.empty
  wmesByVal       <- newTVar $! EnvWmesByVal       Map.empty
  amems           <- newTVar $! EnvAmems           Map.empty
  prods           <- newTVar $! EnvProds           Set.empty

  return Env { envIdState         = idState
             , envSymbolsRegistry = symbolsRegistry
             , envVarsRegistry    = varsRegistry
             , envWmesRegistry    = wmesRegistry
             , envWmesByObj       = wmesByObj
             , envWmesByAttr      = wmesByAttr
             , envWmesByVal       = wmesByVal
             , envAmems           = amems
             , envProds           = prods }

-- GENERATING IDS

-- | Generates a new Id.
genid :: Env -> STM Id
genid Env { envIdState = eid } = do
  EnvIdState recent <- readTVar eid

  -- Hopefully not in a reasonable time
  when (recent == maxBound) (error "Id overflow, can't go on.")

  let new = recent + 1
  writeTVar eid $! EnvIdState new
  return new
{-# INLINE genid #-}

-- SPECIAL SYMBOLS

emptySymbol :: Symbol
emptySymbol =  Symbol (SymbolId (-1)) (SymbolName "")

emptyVariable :: Symbol
emptyVariable =  Variable (VariableId (-2)) (VariableName "?")

-- | A wildcard (any) Symbol representation
wildcardSymbol :: Symbol
wildcardSymbol = Symbol (SymbolId   (-3)) (SymbolName "*")

-- INTERNING SYMBOLS

-- | Interns and returns a Symbol represented by the String argument.
internSymbol :: Env -> String -> STM Symbol
internSymbol env name = case namePred name of
  EmptySymbolName -> return emptySymbol
  OneCharVar      -> return emptyVariable
  OneCharSymbol   -> internStdSymbol env (SymbolName   name)
  MultiCharVar    -> internVariable  env (VariableName name)
  MultiCharSymbol -> internStdSymbol env (SymbolName   name)

data NamePred = EmptySymbolName
              | OneCharVar
              | OneCharSymbol
              | MultiCharVar
              | MultiCharSymbol deriving Show

namePred :: String -> NamePred
namePred ""   = EmptySymbolName
namePred [c]
  | c == '?'  = OneCharVar
  | otherwise = OneCharSymbol
namePred (c:_:_)
  | c == '?'  = MultiCharVar
  | otherwise = MultiCharSymbol
{-# INLINE namePred #-}

internStdSymbol :: Env -> SymbolName -> STM Symbol
internStdSymbol env@Env { envSymbolsRegistry = ereg } name = do
  EnvSymbolsRegistry reg <- readTVar ereg
  case Map.lookup name reg of
    Just s  -> return s
    Nothing -> do
      id' <- genid env
      let s =  Symbol (SymbolId id') name
      writeTVar ereg $! EnvSymbolsRegistry $! Map.insert name s reg
      return s
{-# INLINE internStdSymbol #-}

internVariable :: Env -> VariableName -> STM Symbol
internVariable env@Env { envVarsRegistry = ereg } name = do
  EnvVarsRegistry reg <- readTVar ereg
  case Map.lookup name reg of
    Just s  -> return s
    Nothing -> do
      id' <- genid env
      let v =  Variable (VariableId id') name
      writeTVar ereg $! EnvVarsRegistry $! Map.insert name v reg
      return v
{-# INLINE internVariable #-}
