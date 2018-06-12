{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Control.Eff
  ( -- * Effect base-type
    Internal.Eff
    -- * Effect list
  , OpenUnion.Member
    -- Rest (not included in documentation)
  , module Internal
  , module OpenUnion
  ) where

import Control.Eff.Internal as Internal hiding (Lift(..), lift, runLift)
import Data.OpenUnion as OpenUnion
