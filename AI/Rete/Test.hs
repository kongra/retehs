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
    , tcompose
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

      -- * Options
    , noOpts
    , before
    , after
    , interpose
    )
    where

import AI.Rete.Data
import AI.Rete.Flow
import AI.Rete.Net
import Control.Concurrent.STM
import Control.Monad (void)
import Data.List (permutations, intersperse)

-- TESTING TASKS

-- | Type of a testing task.
type T m = Env -> m ()

-- | Creates a composition of tasks. Composed tasks in the result
-- execute sequentially in order.
tcompose :: Monad m => [T m] -> T m
tcompose ts env = mapM_ (\t -> t env) ts
{-# INLINE tcompose #-}

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
  removeProdT prod       env = void (removeProd env prod        )
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

-- OPTIONS

data Opts m =
  Opts
  {
    optsBefore    :: Maybe (T m)
  , optsAfter     :: Maybe (T m)
  , optsInterpose :: Maybe (T m)
  }

noopts :: Opts m
noopts = Opts Nothing Nothing Nothing

type Switch m = Opts m -> Opts m

-- | Creates a Switch that sets the task to be executed before
-- executing a sequence of tasks.
before :: T m -> Switch m
before t o = o { optsBefore = Just t }
{-# INLINE before #-}

-- | Creates a Switch that sets the task to be executed after
-- executing a sequence of tasks.
after :: T m -> Switch m
after t o = o { optsAfter = Just t }
{-# INLINE after #-}

-- | Creates a Switch that sets the task to interpose the executed
-- sequence of tasks.
interpose :: T m -> Switch m
interpose t o = o { optsInterpose = Just t }
{-# INLINE interpose #-}

noOpts :: Switch m
noOpts _ = noopts
{-# INLINE noOpts #-}

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
execTasks :: Monad m => m Env -> Switch m -> [T m] -> m ()
execTasks env s ts = do
  let opts = s noopts
      ts1  = case optsInterpose opts of
        Nothing -> ts
        Just t  -> intersperse t ts
      ts2 = case optsBefore opts of
        Nothing -> ts1
        Just t  -> t:ts1
      ts3 = case optsAfter opts of
        Nothing -> ts2
        Just t  -> ts2 ++ [t]

  e <- env
  mapM_ (\t -> t e) ts3
{-# INLINE execTasks #-}

-- | Executes tasks sequentially using newly created Env.
execTasksD :: InNewEnv m => Switch m -> [T m] -> m ()
execTasksD s ts = inNewEnv $ \env -> execTasks (return env) s ts
{-# INLINE execTasksD #-}

-- | Executes permutations of tasks. For every permutation we provide
-- an Env.
execPermutedTasks :: Monad m => m Env -> Switch m -> [T m] -> m ()
execPermutedTasks env s = mapM_ (execTasks env s) . permutations
{-# INLINE execPermutedTasks #-}

-- | Executes permutations of tasks. For every permutation a newly
-- created Env is used.
execPermutedTasksD :: InNewEnv m => Switch m -> [T m] -> m ()
execPermutedTasksD s = mapM_ (execTasksD s) . permutations
{-# INLINE execPermutedTasksD #-}
