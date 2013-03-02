-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Persistent
-- Copyright   :  (c) Adam C. Foltzer 2013
-- License     :  BSD3
--
-- Maintainer  :  acfoltzer@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires rank-2 types for runST)
--
-- This library provides support for a persistent version of the
-- 'Control.Monad.ST.ST' monad. Internally, references are backed by a
-- 'Data.IntMap.IntMap', rather than being mutable variables on the
-- heap. This decreases performance, but can be useful in certain
-- settings, particularly those involving backtracking.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Persistent (
    -- * The Persistent 'ST' Monad
    ST
  , runST
    -- * The Persistent 'ST' Monad transformer
  , STT
  , runSTT
  ) where

import Control.Monad.ST.Persistent.Internal

