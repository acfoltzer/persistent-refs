{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.STRef.Persistent (
    STRef
  , MonadRef(..)
  , modifyRef'
  ) where

import Control.Lens
import Control.Monad.Ref
import Control.Monad.ST.Persistent.Internal
import Data.IntMap as IntMap
import Data.Maybe
import Unsafe.Coerce

newtype STRef s a = STRef Int

newSTRef :: Monad m => a -> STT s m (STRef s a)
newSTRef x = do i <- next <<%= (+1)
                heap %= insert i (unsafeCoerce x)
                return (STRef i)

readSTRef :: Monad m => STRef s a -> STT s m a
readSTRef (STRef i) = uses heap (unsafeCoerce . fromJust . IntMap.lookup i)

writeSTRef :: Monad m => STRef s a -> a -> STT s m ()
writeSTRef (STRef i) x = heap %= insert i (unsafeCoerce x)

instance Monad m => MonadRef (STRef s) (STT s m) where
    newRef   = newSTRef
    readRef  = readSTRef
    writeRef = writeSTRef

modifyRef' :: MonadRef r m => r a -> (a -> a) -> m ()
modifyRef' r f = do
    x <- readRef r
    let x' = f x
    x' `seq` writeRef r x'
