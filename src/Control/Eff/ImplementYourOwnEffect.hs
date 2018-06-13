-- | This module exports functions, types, and typeclasses necessary for
-- implementing a custom effect and/or effect handler
module Control.Eff.ImplementYourOwnEffect
  ( -- * Open Unions
    OpenUnion.Union
  , inj
  , prj
  , decomp
  -- , Member already exported in Control.Eff; users should import both
  , SetMember
  , weaken
  ) where

--import Control.Eff.Internal as Internal hiding (Lift(..), lift, runLift)
import Data.OpenUnion as OpenUnion
