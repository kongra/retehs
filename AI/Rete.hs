{-# LANGUAGE    Trustworthy #-}
{-# OPTIONS_GHC -W -Wall    #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-07-21
-- Reworked    : 2015-02-12
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This is the interface part for the Rete algorithm.
------------------------------------------------------------------------
module AI.Rete
    (
      -- * Environment
      Env
    , createEnv

      -- * Symbols
    , Constant
    , Variable
    , Symbol
    , Symbolic

      -- * Conditions
    , c
    , C
    , n
    , N

      -- * Adding/removing 'Wme's
    , addWme
    , removeWme

      -- * Adding/removing productions
    , addProd
    , removeProd

      -- * Actions
    , Action
    , Actx

      -- * Accessing information in actions
    , val
    , valE
    , valM
    , VarVal (..)
    )
    where

import AI.Rete.Data
import AI.Rete.Flow
import AI.Rete.Net