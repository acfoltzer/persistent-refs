{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.ST.Persistent.Internal where

import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.IntMap (IntMap, empty)
import GHC.Base (Any)

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative)
#else
import Control.Applicative (Alternative, Applicative)
#endif

data Heap = Heap { heap :: IntMap Any, next :: Int }

emptyHeap :: Heap
emptyHeap = Heap { heap = empty, next = minBound }

-- | A persistent version of the 'Control.Monad.ST.ST' monad.
type ST s = STT s Identity

-- | Run a computation that uses persistent references, and return a
-- pure value. The rank-2 type offers similar guarantees to
-- 'Control.Monad.ST.runST'.
runST :: (forall s. ST s a) -> a
runST m = runIdentity (runSTT m)

newtype STT s m a = STT (StateT Heap m a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadTrans)

-- | Run a computation that uses persistent references, and return a
-- pure value. The rank-2 type offers similar guarantees to
-- 'Control.Monad.ST.runST'.
runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT (STT c) = evalStateT c emptyHeap
