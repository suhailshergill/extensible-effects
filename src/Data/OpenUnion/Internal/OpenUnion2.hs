{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion2.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
-- This implementation relies on _closed_ type families added to GHC 7.8. It has
-- NO overlapping instances
module Data.OpenUnion.Internal.OpenUnion2( Union (..)
                                         , (:>)
                                         , Member
                                         , MemberU
                                         ) where

import Data.OpenUnion.Internal.Base

-- | Class Member is defined only for the sake of the interface compatibility
-- with OpenUnion1. Generally, the closed type family Member' below could be
-- used instead.
--
-- The @`Member` t r@ specifies whether @t@ is present anywhere in the sum type
-- @r@, where @t@ is some effectful type, e.g. @`Lift` `IO`@, @`State` Int`@.
class (Member' t r ~ True) => Member (t :: * -> *) r
instance (Member' t r ~ True) => Member t r

type family Member' (t :: * -> *) r :: Bool where
  Member' t (t :> r)  = True
  Member' t ()        = False
  Member' t (t' :> r) = Member' t r

type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = True
  EQU a b = False

-- | This class is used for emulating monad transformers
class Member t r => MemberU (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 :> r)) => MemberU tag t1 (t2 :> r)

class Member t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' True tag (tag e) (tag e :> r)
instance (Member' t (t' :> r) ~ True, MemberU tag t r) =>
           MemberU' False tag t (t' :> r)
