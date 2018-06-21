{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#else
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
#endif

-- | A monadic library for implementing effectful computation in a modular way.
--
-- This module provides the @Eff@ monad - the base type for all effectful
-- computation.
-- The @Member@ typeclass is the main interface for describing the .
--
-- Consult the @Control.Eff.QuickStart@ module and the readme for gentle
-- introductions.
--
-- To use extensible effects effectively some language extensions are
-- necessary/suggested.
--
-- @
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- @
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
