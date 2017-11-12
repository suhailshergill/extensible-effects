{-# OPTIONS_HADDOCK hide, show-extensions #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds, FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-} -- GHC.Exts.Constraint makes module not 'Safe'
{-# LANGUAGE UndecidableSuperClasses #-}

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif

-- | Adapted from <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs> and
-- <http://okmij.org/ftp/Haskell/extensible/OpenUnion2.hs>
module Data.OpenUnion.Internal.Base(
  -- * Datatypes
  Union (..)
  , (:>)
  -- * Classes
  , MemberConstraint
  , MemberImpl
  -- ** Monad transformer related
  , MemberUConstraint
  , MemberUImpl
  ) where

import Data.Typeable
import GHC.Exts (Constraint)

-- | Parameter @r@ is phantom: it just tells what could be in the union.
-- Where @r@ is @t1 :> t2 ... :> tn@, @`Union` r v@ can be constructed with a
-- value of type @ti v@.
-- Ideally, we should be able to add the constraint @`Member` t r@.
--
-- NOTE: exposing the constructor below allows users to bypass the type
-- system. See 'Data.OpenUnion.unsafeReUnion' for example.
data Union (r :: *) (v :: *) =
  forall t. (Functor t, Typeable1 t) => Union (t v)
  deriving Typeable

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (Union v) = Union (fmap f v)

-- | A sum data type, for composing effects
infixr 1 :>
data ((a :: * -> *) :> (b :: *))
#if __GLASGOW_HASKELL__ >= 708
  deriving Typeable
#endif

type family MemberConstraint impl (t :: * -> *) r :: Constraint
-- | The @`MemberImpl` impl t r@ specifies whether @t@ is present anywhere in
-- the sum type @r@, where @t@ is some effectful type
-- (e.g. @`Lift` `IO`@, @`State` Int`@), for a particular implementation ('impl'
-- label, eg. 'OU1' representing 'OpenUnion1').
class (MemberConstraint impl t r) => MemberImpl impl (t :: * -> *) r

type family MemberUConstraint impl (t :: * -> *) r :: Constraint
-- | This class is used for emulating monad transformers
class MemberUConstraint impl (t :: * -> *) r
      => MemberUImpl impl (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
