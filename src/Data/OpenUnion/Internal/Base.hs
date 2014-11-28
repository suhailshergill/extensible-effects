{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#endif

-- | Adapted from <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs> and
-- <http://okmij.org/ftp/Haskell/extensible/OpenUnion2.hs>
module Data.OpenUnion.Internal.Base( Union (..)
                                   , (:>)
                                   ) where

import Data.Typeable

-- | Parameter @r@ is phantom: it just tells what could be in the union.
-- Where @r@ is @t1 :> t2 ... :> tn@, @`Union` r v@ can be constructed with a
-- value of type @ti v@.
-- Ideally, we should be able to add the constraint @`Member` t r@.
--
-- NOTE: exposing the constructor below allows users to bypass the type
-- system. See 'Data.OpenUnion.unsafeReUnion' for example.
data Union r v = forall t. (Functor t, Typeable1 t) => Union (t v)

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (Union v) = Union (fmap f v)

-- | A sum data type, for composing effects
infixr 1 :>
data ((a :: * -> *) :> b)
