{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Flow.Symbols
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-22
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
------------------------------------------------------------------------
module AI.Rete.Flow.Symbols
    (
      internSymbol
    , InternedSymbol
    , internedSymbol
    , wildcardSymbol
    )
    where

import           AI.Rete.Data
import           AI.Rete.Flow.Genid
import           Control.Concurrent.STM
import qualified Data.HashMap.Strict as Map

-- SPECIAL SYMBOLS

emptySymbol :: Symbol
emptySymbol =  Symbol (SymbolId (-1)) (SymbolName   "")

emptyVariable :: Symbol
emptyVariable =  Variable (VariableId (-2)) (VariableName "?")

-- | A wildcard (any) Symbol representation
wildcardSymbol :: Symbol
wildcardSymbol = Symbol (SymbolId   (-3)) (SymbolName   "*")

-- INTERNING SYMBOLS

newtype InternedSymbol = InternedSymbol { internedSymbol :: Symbol }

-- | Interns and returns a Symbol represented by the String argument.
internSymbol :: Env -> String -> STM InternedSymbol
internSymbol env name = case namePred name of
  EmptyName       -> return InternedSymbol { internedSymbol = emptySymbol   }
  OneCharVar      -> return InternedSymbol { internedSymbol = emptyVariable }
  OneCharSymbol   -> internStdSymbol env (SymbolName   name)
  MultiCharVar    -> internVariable  env (VariableName name)
  MultiCharSymbol -> internStdSymbol env (SymbolName   name)

data NamePred = EmptyName
              | OneCharVar
              | OneCharSymbol
              | MultiCharVar
              | MultiCharSymbol deriving Show

namePred :: String -> NamePred
namePred ""   = EmptyName
namePred [c]
  | c == '?'  = OneCharVar
  | otherwise = OneCharSymbol
namePred (c:_:_)
  | c == '?'  = MultiCharVar
  | otherwise = MultiCharSymbol
{-# INLINE namePred #-}

internStdSymbol :: Env -> SymbolName -> STM InternedSymbol
internStdSymbol env@Env { envSymbolsRegistry = ereg } name = do
  EnvSymbolsRegistry reg <- readTVar ereg
  case Map.lookup name reg of
    Just s  -> return (InternedSymbol s)
    Nothing -> do
      gid   <- genid env
      let s =  Symbol (SymbolId (genidVal gid)) name
      writeTVar ereg $! EnvSymbolsRegistry (Map.insert name s reg)
      return (InternedSymbol s)

internVariable :: Env -> VariableName -> STM InternedSymbol
internVariable env@Env { envVarsRegistry = ereg } name = do
  EnvVarsRegistry reg <- readTVar ereg
  case Map.lookup name reg of
    Just s  -> return (InternedSymbol s)
    Nothing -> do
      gid   <- genid env
      let v =  Variable (VariableId (genidVal gid)) name
      writeTVar ereg $! EnvVarsRegistry (Map.insert name v reg)
      return (InternedSymbol v)
