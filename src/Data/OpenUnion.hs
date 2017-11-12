{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- Only for SetMember below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif
-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs>
-- and <http://okmij.org/ftp/Haskell/extensible/OpenUnion2.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
--
-- TODO: see if we can do away with Typeable constraints, perhaps by
-- incorporating ideas from <http://okmij.org/ftp/Haskell/extensible/TList.hs>
module Data.OpenUnion(
  -- * Classes
  -- | @`Member` t r@ specifies whether @t@ is present anywhere in the sum type
  -- @r@, where @t@ is some effectful type, e.g. @`Lift` `IO`@, @`State` Int`@
  Member
  -- ** Monad transformer related
  -- | @`SetMember` set t r@ is used to emulate monad transformers.
  , SetMember
    -- * Type-indexed co-product
    -- ** Datatypes
  , Union
  , (:>)
    -- ** Functions
  , inj
  , prj
  , prjForce
  , decomp
  , unsafeReUnion
  , weaken
  ) where

import Data.OpenUnion.Imports
import Data.Typeable

infixl 4 <?>

-- | infix form of `fromMaybe`.
(<?>) :: Maybe a -> a -> a
Just a <?> _ = a
_ <?> a = a

-- | for the sake of @gcast1@ used below in @`prj`@
newtype Id a = Id { runId :: a }
  deriving Typeable

-- | `SetMember` is similar to `Member`, but it allows types to belong to a
-- \"set\". For every set, only one member can be in @r@ at any given time.
-- This allows us to specify exclusivity and uniqueness among arbitrary effects:
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
class (Member t r) => SetMember set (t :: * -> *) r | r set -> t
instance (MemberU set t r) => SetMember set t r

{-# INLINE inj #-}
-- | Construct a Union.
inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
inj = Union

{-# INLINE prj #-}
-- | Try extracting the contents of a Union as a given type.
prj :: (Typeable1 t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) = runId `fmap` gcast1 (Id v)

{-# INLINE prjForce #-}
-- | Extract the contents of a Union as a given type.
-- If the Union isn't of that type, a runtime error occurs.
prjForce :: (Typeable1 t, Member t r) => Union r v -> (t v -> a) -> a
prjForce u f = f `fmap` prj u <?> error "prjForce with an invalid type"

{-# INLINE decomp #-}
-- | Try extracting the contents of a Union as a given type.
-- If we can't, return a reduced Union that excludes the type we just checked.
decomp :: Typeable1 t => Union (t :> r) v -> Either (Union r v) (t v)
decomp u = Right `fmap` prj u <?> Left (unsafeReUnion u)

{-# INLINE weaken #-}
weaken :: (Typeable1 t, Functor t) => Union r w -> Union (t :> r) w
weaken (Union x) = Union x

{-# INLINE unsafeReUnion #-}
-- | Juggle types for a Union. Use cautiously.
unsafeReUnion :: Union r w -> Union t w
unsafeReUnion (Union v) = Union v
