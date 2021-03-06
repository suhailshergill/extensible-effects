{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module exports functions, types, and typeclasses necessary for
-- implementing a custom effect and/or effect handler.
--

module Control.Eff.Extend
  ( -- * The effect monad
    Eff(..)
  , run
  , eff
    -- * Lifting operations
  , Lift(..), Lifted, LiftedBase
  , lift, runLift
  , catchDynE
  , HandlerDynE(..), catchesDynE
    -- * Open Unions
  , OpenUnion.Union
  , OpenUnion.Member
  , inj
  , prj, pattern OpenUnion.U0'
  , decomp, pattern OpenUnion.U0, pattern OpenUnion.U1
  , SetMember
  , weaken
  -- * Helper functions that are used for implementing effect-handlers
  , Handle(..)
  , Open
  , Relay(..)
  , handle_relay', respond_relay'
  , handle_nat_lifted', handle_nat'
  , raise
  , send
  -- * Arrow types and compositions
  , Arr
  , Arrs
  , first
  , singleK
  , qApp
  , (^$)
  , arr
  , ident
  , comp
  , (^|>)
  , qComp
  , qComps
  )
where

import           Data.OpenUnion                as OpenUnion
import           Control.Eff.Internal
