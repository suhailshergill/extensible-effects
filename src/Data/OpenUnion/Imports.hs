{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module Data.OpenUnion.Imports( Applicative(..)
                             , module Impl
                             ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative(Applicative(..))
#endif
#if __GLASGOW_HASKELL__ >= 708
import Data.OpenUnion.Internal.OpenUnion2 as Impl
#else
import Data.OpenUnion.Internal.OpenUnion1 as Impl
#endif
