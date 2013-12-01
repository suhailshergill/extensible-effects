{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
-- This implementation relies on _closed_ overlapping instances
-- (or closed type function overlapping soon to be added to GHC).
module Data.OpenUnion1( Union
                      , SetMember
                      , Member
                      , (:>)
                      , inj
                      , prj
                      , prjForce
                      , decomp
                      , unsafeReUnion
                      ) where

import Control.Applicative ((<$>))
import Data.Typeable

infixl 4 <?>

-- | infix form of `fromMaybe`.
(<?>) :: Maybe a -> a -> a
Just a <?> _ = a
_ <?> a = a

-- for the sake of gcast1
newtype Id a = Id { runId :: a }
  deriving Typeable

-- | Where @r@ is @t1 :> t2 ... :> tn@, @`Union` r v@ can be constructed with a
-- value of type @ti v@.
-- Ideally, we should be be able to add the constraint @`Member` t r@.
data Union r v = forall t. (Functor t, Typeable1 t) => Union (t v)

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (Union v) = Union (fmap f v)

-- | A sum data type, for composing effects
infixr 1 :>
data ((a :: * -> *) :> b)

-- | The @`Member` t r@ determines whether @t@ is anywhere in the sum type @r@.
class Member t r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

-- | There's a @`SetMember` as t r@ instance if @t@ is an element of the sum
-- datatype @r@. The @as@ type parameter allows types to qualify themselves as
-- members of various "sets", by taking advantage of the @r as -> t@ fundep:
--
-- > -- Terminal effects (effects which must be run last)
-- > data Terminal
-- >
-- > -- Make Lifts part of the Terminal effects set.
-- > -- The fundep assures that there can only be one Terminal effect for any r.
-- > instance Member (Lift m) r => SetMember Terminal (Lift m) r
-- >
-- > -- Only allow a single unique Lift effect, by making a "Lift" set.
-- > instance Member (Lift m) r => SetMember Lift (Lift m) r
class Member t r => SetMember as (t :: * -> *) r | r as -> t
instance SetMember t t (t :> r)
instance SetMember as t r => SetMember as t (t' :> r)

{-# INLINE inj #-}
-- | Construct a Union.
inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
inj = Union

{-# INLINE prj #-}
-- | Try extracting the contents of a Union as a given type.
prj :: (Typeable1 t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) = runId <$> gcast1 (Id v)

{-# INLINE prjForce #-}
-- | Extract the contents of a Union as a given type.
-- If the Union isn't of that type, a runtime error occurs.
prjForce :: (Typeable1 t, Member t r) => Union r v -> (t v -> a) -> a
prjForce u f = f <$> prj u <?> error "prjForce with an invalid type"

{-# INLINE decomp #-}
-- | Try extracting the contents of a Union as a given type.
-- If we can't, return a reduced Union that excludes the type we just checked.
decomp :: Typeable1 t => Union (t :> r) v -> Either (Union r v) (t v)
decomp u = Right <$> prj u <?> Left (unsafeReUnion u)

{-# INLINE unsafeReUnion #-}
-- | Juggle types for a Union. Use cautiously.
unsafeReUnion :: Union r w -> Union t w
unsafeReUnion (Union v) = Union v
