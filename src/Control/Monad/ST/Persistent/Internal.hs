{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
module Control.Monad.ST.Persistent.Internal where

import Control.Applicative (Applicative)
import Control.Lens
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.IntMap
import GHC.Base

data Heap = Heap { _heap :: IntMap Any, _next :: Int }
makeLenses ''Heap

emptyHeap :: Heap
emptyHeap = Heap { _heap = empty, _next = minBound }

type ST s a = STT s Identity a

runST :: (forall s. ST s a) -> a
runST (STT c) = runIdentity $ evalStateT c emptyHeap

newtype STT s m a = STT (StateT Heap m a)
    deriving (Functor, Applicative, Monad, MonadState Heap, MonadIO)

runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT (STT c) = evalStateT c emptyHeap
