{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#else
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
#endif

-- | The main entry-point of this library.
--
-- This module provides the @Eff@ monad - the base monad for all effectful
-- computation.
--

module Control.Eff
  ( -- * Effect base-type
    Internal.run
  , Internal.Eff
    -- * Effect list
  , OpenUnion.Member
  , type(<::)
    -- Rest (not included in documentation somehow)
  , module Internal
  , module OpenUnion
  ) where

import Control.Eff.Internal as Internal hiding (Lift(..), lift, runLift)
import Data.OpenUnion as OpenUnion
