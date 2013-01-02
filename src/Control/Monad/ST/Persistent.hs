{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.ST.Persistent where

import Control.Lens
import Control.Monad.State
import Data.IntMap as IntMap
import Data.Maybe
import GHC.Base
import Unsafe.Coerce

data Store = Store { _heap :: IntMap Any, _next :: Int }
makeLenses ''Store

emptyStore :: Store
emptyStore = Store { _heap = empty, _next = minBound }

newtype ST s a = ST (State Store a)
    deriving (Monad, MonadState Store)

runST :: (forall s. ST s a) -> a
runST (ST m) = evalState m emptyStore

-- STRefs

newtype STRef s a = STRef Int

newSTRef :: a -> ST s (STRef s a)
newSTRef x = do i <- next <<%= (+1)
                heap %= insert i (unsafeCoerce x)
                return (STRef i)

readSTRef :: STRef s a -> ST s a
readSTRef (STRef i) = uses heap (unsafeCoerce . fromJust . IntMap.lookup i)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef i) x = heap %= insert i (unsafeCoerce x)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = writeSTRef ref . f =<< readSTRef ref

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
    x <- readSTRef ref
    let x' = f x
    x' `seq` writeSTRef ref x'