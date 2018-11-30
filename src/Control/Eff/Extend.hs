-- | This module exports functions, types, and typeclasses necessary for
-- implementing a custom effect and/or effect handler.
--

module Control.Eff.Extend
  ( -- * The effect monad
    Eff(..)
  , run
  , eff, impurePrj, impureDecomp
    -- * Lifting operations
  , Lift(..), Lifted, LiftedBase
  , lift, runLift
  , catchDynE
  , HandlerDynE(..), catchesDynE
    -- * Open Unions
  , OpenUnion.Union
  , OpenUnion.Member
  , inj
  , prj
  , decomp
  , SetMember
  , weaken
  -- * Helper functions that are used for implementing effect-handlers
  , Handle, handle
  , Relay, relay
  , handle_relay
  , handle_relay'
  , interpose
  , interpose'
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
  , qComps, (^|$^)
  , (~^), qThen, andThen
  )
where

import           Data.OpenUnion                as OpenUnion
import           Control.Eff.Internal
