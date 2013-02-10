{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.ST.Persistent.Internal where

import Control.Applicative (Applicative)
import Control.Lens
import Control.Monad.State.Strict
import Data.IntMap
import GHC.Base

data Heap = Heap { _heap :: IntMap Any, _next :: Int }
makeLenses ''Heap

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
