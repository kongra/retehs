{-# LANGUAGE    Trustworthy           #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    RankNTypes            #-}
{-# OPTIONS_GHC -W -Wall              #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Test
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-02-24
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
------------------------------------------------------------------------

module AI.Rete.Test
    (
      -- * Testing tasks
      T
    , addWmeT
    , removeWmeT
    , addProdT
    , addProdRT
    , removeProdT

      -- * Tasks execution
    , inNewEnv
    , execTasks
    , execTasksD
    , execPermutedTasks
    , execPermutedTasksD
    )
    where

import AI.Rete.Data
import AI.Rete.Flow
import AI.Rete.Net
import Control.Concurrent.STM
import Control.Monad (void)
import Data.List (permutations)

-- TESTING TASKS

-- | Type of a testing task.
type T m = Env -> m ()

class Monad m => WmeT m where
  -- | Task that adds a Wme.
  addWmeT    :: (ToConstant o, ToConstant a, ToConstant v) => o -> a -> v -> T m

  -- | Task that removes a Wme.
  removeWmeT :: (ToConstant o, ToConstant a, ToConstant v) => o -> a -> v -> T m

instance WmeT STM where
  addWmeT    o a v env = void (addWme    env o a v)
  removeWmeT o a v env = void (removeWme env o a v)
  {-# INLINE addWmeT    #-}
  {-# INLINE removeWmeT #-}

instance WmeT IO where
  addWmeT    o a v env = atomically (addWmeT    o a v env)
  removeWmeT o a v env = atomically (removeWmeT o a v env)
  {-# INLINE addWmeT    #-}
  {-# INLINE removeWmeT #-}

class Monad m => ProdT m where
  -- | Task that adds a Prod.
  addProdT    :: C -> [C] -> [N] -> Action -> T m

  -- | Task that adds a Prod with a revoke Action.
  addProdRT   :: C -> [C] -> [N] -> Action -> Action -> T m

  -- | Task that removes a Prod.
  removeProdT :: Prod -> T m

instance ProdT STM where
  addProdT  c' cs ns a   env = void (addProd    env c' cs ns a  )
  addProdRT c' cs ns a r env = void (addProdR   env c' cs ns a r)
  removeProdT prod       env = void (removeProd env prod)
  {-# INLINE addProdT    #-}
  {-# INLINE addProdRT   #-}
  {-# INLINE removeProdT #-}

instance ProdT IO where
  addProdT  c' cs ns a   env = atomically (addProdT    c' cs ns a   env)
  addProdRT c' cs ns a r env = atomically (addProdRT   c' cs ns a r env)
  removeProdT prod       env = atomically (removeProdT prod         env)
  {-# INLINE addProdT    #-}
  {-# INLINE addProdRT   #-}
  {-# INLINE removeProdT #-}

-- TASKS EXECUTION

class Monad m => InNewEnv m where
  -- | Executes an action in a newly created Env.
  inNewEnv :: (Env -> m a) -> m a

instance InNewEnv IO where
  inNewEnv f = atomically createEnv >>= f
  {-# INLINE inNewEnv #-}

instance InNewEnv STM where
  inNewEnv f = createEnv >>= f
  {-# INLINE inNewEnv #-}

-- | Executes tasks sequentially using Env.
execTasks :: Monad m => m Env -> [T m] -> m ()
execTasks env ts = do
  e <- env
  mapM_ (\t -> t e) ts
{-# INLINE execTasks #-}

-- | Executes tasks sequentially using newly created Env.
execTasksD :: InNewEnv m => [T m] -> m ()
execTasksD ts = inNewEnv $ \env -> execTasks (return env) ts
{-# INLINE execTasksD #-}

-- | Executes permutations of tasks. For every permutation we provide
-- an Env.
execPermutedTasks :: Monad m => m Env -> [T m] -> m ()
execPermutedTasks env = mapM_ (execTasks env) . permutations
{-# INLINE execPermutedTasks #-}

-- | Executes permutations of tasks. For every permutation a newly
-- created Env is used.
execPermutedTasksD :: InNewEnv m => [T m] -> m ()
execPermutedTasksD = mapM_ execTasksD . permutations
{-# INLINE execPermutedTasksD #-}

-- test1 :: IO ()
-- test1 = do
--   atomically $ execPermutedTasksD
--     [ addWmeT "sójka"  "jestPtak" True
--     , addWmeT "wróbel" "jestPtak" True
--     ]

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

--   -- _    <- atomically $

--   -- _    <- atomically $ addWme env
--   --         "wróbel" "jestPtak" True

--   -- _    <- atomically $ addWme env
--   --         "sikorka" "jestPtak" True

--   -- _    <- atomically $ addProd env
--   --         (c (var "p") "jestPtak" True)
--   --         []
--   --         []
--           -- (acompose [ traceTokActionD "tok: "
--           --           -- , traceVarAction    "p: " (var "p")
--           --           ])

--   -- atomically (toString boundless opts env) >>= putStrLn
--   return ()
