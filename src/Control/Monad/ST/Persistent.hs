{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.ST.Persistent (
    ST
  , runST
  , STT
  , runSTT
  ) where

import Control.Monad.ST.Persistent.Internal

