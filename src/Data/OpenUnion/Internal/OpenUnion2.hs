{-# OPTIONS_HADDOCK hide, show-extensions #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion2.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
-- This implementation relies on _closed_ type families added to GHC 7.8. It has
-- NO overlapping instances
module Data.OpenUnion.Internal.OpenUnion2( OU2
                                         , module Base
                                         ) where

import Data.OpenUnion.Internal.Base as Base

-- | Label to represent 'OpenUnion1' implementation
data OU2

-- | Instance MemberImpl is defined only for the sake of the interface
-- compatibility with OpenUnion1. Generally, the closed type family Member'
-- below could be used instead.
--
-- @`MemberImpl` OU2 t r@ specifies whether @t@ is present anywhere in the sum
-- type @r@, where @t@ is some effectful type
type instance MemberConstraint OU2 t r = (Member' t r ~ 'True)
instance (MemberConstraint OU2 t r) => MemberImpl OU2 t r

type family Member' (t :: * -> *) r :: Bool where
  Member' t (t :> r)  = 'True
  Member' t ()        = 'False
  Member' t (t' :> r) = Member' t r

type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = 'True
  EQU a b = 'False

-- | For emulating monad transformers
type instance MemberUConstraint OU2 t r = (MemberImpl OU2 t r)
instance (MemberU' (EQU t1 t2) tag t1 (t2 :> r)) => MemberUImpl OU2 tag t1 (t2 :> r)

class MemberImpl OU2 t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' 'True tag (tag e) (tag e :> r)
instance (Member' t (t' :> r) ~ 'True, MemberUImpl OU2 tag t r) =>
           MemberU' 'False tag t (t' :> r)
