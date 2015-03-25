{-# LANGUAGE    Trustworthy           #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    RankNTypes            #-}
{-# OPTIONS_GHC -W -Wall              #-}
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
import AI.Rete.Test
import Control.Concurrent.STM
import Debug.Trace (traceM)

opts :: Switch
opts = nonVerboseData . netTopDown

test1 :: IO ()
test1 = do
  let printEnv env = do
        s <- toString boundless opts env
        traceM s

  atomically $ execPermutedTasksD
    -- (after printEnv)
    noOpts

    [ addWmeT "sójka"  "jestPtak" True
    , addWmeT "wróbel" "jestPtak" True
    , addWmeT "kawka"  "jestPtak" True

    , addProdT
      ( c "wróbel" "jestPtak" True)
      [ c "sójka"  "jestPtak" True ]
      []
      (traceTokActionD "tok: ")
    ]

-- test3 :: IO ()
-- test3 = do
--   let opts = nonVerboseData . netTopDown

--   inNewEnv $ \env -> do
--     atomically $ execPermutedTasks
--       [ void $ addWme env "sójka"  "jestPtak" True
--       , void $ addWme env "kawka"  "jestPtak" True
--       , void $ addWme env "wróbel" "jestPtak" True

--       , void $ addProd env
--         (c "wróbel" "jestPtak" True) [] []
--         (traceTokActionD "tok: ")
--       ]
--     atomically (toString boundless opts env) >>= putStrLn

  -- _    <- atomically $

  -- _    <- atomically $ addWme env
  --         "wróbel" "jestPtak" True

  -- _    <- atomically $ addWme env
  --         "sikorka" "jestPtak" True

  -- _    <- atomically $ addProd env
  --         (c (var "p") "jestPtak" True)
  --         []
  --         []
          -- (acompose [ traceTokActionD "tok: "
          --           -- , traceVarAction    "p: " (var "p")
          --           ])

  -- atomically (toString boundless opts env) >>= putStrLn
  -- return ()

-- test2 :: IO ()
-- test2 = do
--   env  <- atomically createEnv

--   _ <- atomically $ addProdR env
--        ( c (var "x") leftOf "z")
--        [                       ]
--        [ n (var "x") on     "y"]

--        (traceAction "success")
--        (traceAction "too early, my dear")

--   let opts = nonVerboseData . netTopDown

--   atomically (toString boundless opts env) >>= putStrLn

--   _ <- atomically $ addWme env "a" leftOf "z"
--   atomically (toString boundless opts env) >>= putStrLn

--   _ <- atomically $ addWme env "a" on     "y"
--   atomically (toString boundless opts env) >>= putStrLn

--   return ()

-- test1 :: IO ()
-- test1 = do
--   env  <- atomically createEnv

--   _ <- atomically $ addProd env
--        ( c (var "x") (var "y") (var "z"))
--        [ c (var "x") (var "z") (var "y")
--        , c (var "y") (var "x") (var "z")
--        , c (var "y") (var "z") (var "x")
--        , c (var "z") (var "x") (var "y")
--        , c (var "z") (var "y") (var "x") ]
--        noNegs
--        (traceAction "success")

--   -- repr <- atomically $ toString boundless soleNetTopDown env
--   -- putStrLn repr

--   _ <- atomically $ addWme env "a" "b" "c"
--   _ <- atomically $ addWme env "a" "c" "b"
--   _ <- atomically $ addWme env "b" "a" "c"
--   _ <- atomically $ addWme env "b" "c" "a"
--   _ <- atomically $ addWme env "c" "a" "b"
--   _ <- atomically $ addWme env "c" "b" "a"

--   return ()

-- teścik :: IO ()
-- teścik = do
--   env  <- atomically createEnv

--   _    <- atomically $
--           addWme env b1 on "table"

--   prod <- atomically $
--           addProd env
--                   (c (var "<x>") on (var "<y>"))
--                   noMoreConds
--                   noNegs
--                   (traceAction "success")

--   repr <- atomically $ toString boundless soleNetTopDown env
--   putStrLn repr

--   return ()

-- b1, b2, b3, b4 :: NamedPrimitive
-- b1     = NamedPrimitive (IntPrimitive 1 ) "b1"
-- b2     = NamedPrimitive (IntPrimitive 2 ) "b2"
-- b3     = NamedPrimitive (IntPrimitive 3 ) "b3"
-- b4     = NamedPrimitive (IntPrimitive 4 ) "b4"

-- on, color, red, blue, table, leftOf :: NamedPrimitive
-- on     = NamedPrimitive (IntPrimitive 5 ) "on"
-- color  = NamedPrimitive (IntPrimitive 6 ) "color"
-- red    = NamedPrimitive (IntPrimitive 7 ) "red"
-- blue   = NamedPrimitive (IntPrimitive 8 ) "blue"
-- table  = NamedPrimitive (IntPrimitive 9 ) "table"
-- leftOf = NamedPrimitive (IntPrimitive 10) "leftOf"
