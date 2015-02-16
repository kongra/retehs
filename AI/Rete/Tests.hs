{-# LANGUAGE    Trustworthy #-}
{-# OPTIONS_GHC -W -Wall    #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Tests
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-02-16
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
------------------------------------------------------------------------

module AI.Rete.Tests where

import AI.Rete
import AI.Rete.Print
import Control.Concurrent.STM

b1, b2, b3, b4 :: NamedPrimitive
b1     = NamedPrimitive (IntPrimitive 1)  "b1"
b2     = NamedPrimitive (IntPrimitive 2)  "b2"
b3     = NamedPrimitive (IntPrimitive 3)  "b3"
b4     = NamedPrimitive (IntPrimitive 4)  "b4"

on, color, red, blue, table, leftOf :: NamedPrimitive
on     = NamedPrimitive (IntPrimitive 5)  "on"
color  = NamedPrimitive (IntPrimitive 6)  "color"
red    = NamedPrimitive (IntPrimitive 7)  "red"
blue   = NamedPrimitive (IntPrimitive 8)  "blue"
table  = NamedPrimitive (IntPrimitive 9)  "table"
leftOf = NamedPrimitive (IntPrimitive 10) "leftOf"

teścik :: IO ()
teścik = do
  env  <- atomically createEnv

  _    <- atomically $
          addWme env b1 on "table"

  prod <- atomically $
          addProd env
                  (c (var "<x>") on (var "<y>"))
                  noMoreConds
                  noNegs
                  (traceAction "success")

  repr <- atomically $ toString boundless soleNetTopDown env
  putStrLn repr

  return ()
