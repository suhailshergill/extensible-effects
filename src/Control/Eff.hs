{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Control.Eff
  ( -- * Effect base-type
    -- | The the monad that contains all effects --TODO: comment
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
