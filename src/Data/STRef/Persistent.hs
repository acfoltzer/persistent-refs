{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef.Persistent
-- Copyright   :  (c) Adam C. Foltzer 2013
-- License     :  BSD3
--
-- Maintainer  :  acfoltzer@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires rank-2 types for runST)
--
-- Mutable references in the persistent
-- 'Control.Monad.ST.Persistent.ST' monad.
--
-----------------------------------------------------------------------------

module Data.STRef.Persistent (
    -- * 'STRef's
    STRef
    -- * 'MonadRef' Operations
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

newSTRef :: a -> ST s (STRef s a)
newSTRef x = ST $ do 
    i <- next <<%= (+1)
    heap %= insert i (unsafeCoerce x)
    return (STRef i)

readSTRef :: STRef s a -> ST s a
readSTRef (STRef i) = 
    ST $ uses heap (unsafeCoerce . fromJust . IntMap.lookup i)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef i) x = 
    ST $ heap %= insert i (unsafeCoerce x)

instance MonadRef (STRef s) (ST s) where
    newRef   = newSTRef
    readRef  = readSTRef
    writeRef = writeSTRef

modifyRef' :: MonadRef r m => r a -> (a -> a) -> m ()
modifyRef' r f = do
    x <- readRef r
    let x' = f x
    x' `seq` writeRef r x'
