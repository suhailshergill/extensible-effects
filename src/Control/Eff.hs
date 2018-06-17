{-# OPTIONS_GHC -Wno-duplicate-exports #-}

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
