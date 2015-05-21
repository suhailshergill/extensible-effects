{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- For 'Member' and 'MemberU' aliases below
{-# LANGUAGE ConstraintKinds, PolyKinds #-}

module Data.OpenUnion.Imports( Applicative(..)
                             , module Impl
                             , Member
                             , MemberU
                             ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative(Applicative(..))
#endif
#if __GLASGOW_HASKELL__ >= 708
import Data.OpenUnion.Internal.OpenUnion2 as Impl
type Member = MemberImpl OU2
type MemberU = MemberUImpl OU2
#else
import Data.OpenUnion.Internal.OpenUnion1 as Impl
type Member = MemberImpl OU1
type MemberU = MemberUImpl OU1
#endif
