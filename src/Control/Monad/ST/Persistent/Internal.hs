{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-splices #-}
module Control.Monad.ST.Persistent.Internal where

import Control.Applicative (Applicative)
import Control.Lens
import Control.Monad.State.Strict
import Data.IntMap
import GHC.Base

data Heap = Heap { _heap :: IntMap Any, _next :: Int }
-- makeLenses ''Heap

-- manually using splices because TH before 7.6 can't handle Any
heap :: Lens' Heap (IntMap Any)
heap _f_a1T2 (Heap __heap'_a1T3 __next_a1T5)
  = ((\ __heap_a1T4 -> Heap __heap_a1T4 __next_a1T5)
     `fmap` (_f_a1T2 __heap'_a1T3))
{-# INLINE heap #-}
next :: Lens' Heap Int
next _f_a1T6 (Heap __heap_a1T7 __next'_a1T8)
  = ((\ __next_a1T9 -> Heap __heap_a1T7 __next_a1T9)
     `fmap` (_f_a1T6 __next'_a1T8))
{-# INLINE next #-}

emptyHeap :: Heap
emptyHeap = Heap { _heap = empty, _next = minBound }

-- | A persistent version of the 'Control.Monad.ST.ST' monad.
newtype ST s a = ST (State Heap a)
    deriving (Functor, Applicative, Monad)

-- | Run a computation that uses persistent references, and return a
-- pure value. The rank-2 type offers similar guarantees to
-- 'Control.Monad.ST.runST'.
runST :: (forall s. ST s a) -> a
runST (ST c) = evalState c emptyHeap

-- I'm not sure whether the semantics of this transformer actually
-- make any sense, so I'm not exporting this for now...

newtype STT s m a = STT (StateT Heap m a)
    deriving (Functor, Applicative, Monad, MonadIO)

runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT (STT c) = evalStateT c emptyHeap
