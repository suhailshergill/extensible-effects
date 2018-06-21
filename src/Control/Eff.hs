{-# LANGUAGE ExplicitNamespaces #-}

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
-- {-# LANGUAGE MonoLocalBinds #-}
-- @
--

module Control.Eff
  ( -- * Effect base-type
    Internal.run
  , Internal.Eff
    -- * Effect list
  , OpenUnion.Member
  , OpenUnion.SetMember
  , type(<::)
  ) where

import Control.Eff.Internal as Internal
import Data.OpenUnion as OpenUnion
