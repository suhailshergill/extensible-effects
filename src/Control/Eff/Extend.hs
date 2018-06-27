-- | This module exports functions, types, and typeclasses necessary for
-- implementing a custom effect and/or effect handler.
--

module Control.Eff.Extend
  ( -- * The effect monad
    Eff(..)
  , run
    -- * Open Unions
  , OpenUnion.Union
  , OpenUnion.Member
  , inj
  , prj
  , decomp
  , SetMember
  , weaken
  -- * Helper functions that are used for implementing effect-handlers
  , handle_relay
  , handle_relay_s
  , interpose
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
