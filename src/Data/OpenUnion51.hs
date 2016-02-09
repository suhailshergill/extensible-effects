{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -Wwarn #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- | Open unions (type-indexed co-products) for extensible effects
-- All operations are constant-time, and there is no Typeable constraint
--
-- This is a variation of OpenUion5.hs, which relies on overlapping
-- instances instead of closed type families. Closed type families
-- have their problems: overlapping instances can resolve even
-- for unground types, but closed type families are subject to a
-- strict apartness condition.
--
-- This implementation is very similar to OpenUnion1.hs, but without
-- the annoying Typeable constraint. We sort of emulate it:
--
-- Our list r of open union components is a small Universe.
-- Therefore, we can use the Typeable-like evidence in that
-- universe. We hence can define
--
-- @
-- data Union r v where
--   Union :: t v -> TRep t r -> Union r v -- t is existential
-- @
-- where
--
-- @
-- data TRep t r where
--   T0 :: TRep t (t ': r)
--   TS :: TRep t r -> TRep (any ': r)
-- @
-- Then Member is a type class that produces TRep
-- Taken literally it doesn't seem much better than
-- OpenUinion41.hs. However, we can cheat and use the index of the
-- type t in the list r as the TRep. (We will need UnsafeCoerce then).

-- The interface is the same as of other OpenUnion*.hs
module Data.OpenUnion51 (Union, inj, prj, decomp,
                   Member, MemberU2, weaken
                  ) where

import Unsafe.Coerce(unsafeCoerce)

-- The data constructors of Union are not exported

-- Strong Sum (Existential with the evidence) is an open union
-- t is can be a GADT and hence not necessarily a Functor.
-- Int is the index of t in the list r; that is, the index of t in the
-- universe r
data Union (r :: [ * -> * ]) v where
  Union :: {-# UNPACK #-} !Int -> t v -> Union r v

{-# INLINE prj' #-}
{-# INLINE inj' #-}
inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

newtype P t r = P{unP :: Int}

class (FindElem t r) => Member (t :: * -> *) r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

{-
-- Optimized specialized instance
instance Member t '[t] where
  {-# INLINE inj #-}
  {-# INLINE prj #-}
  inj x           = Union 0 x
  prj (Union _ x) = Just (unsafeCoerce x)
-}

instance (FindElem t r) => Member t r where
  {-# INLINE inj #-}
  {-# INLINE prj #-}
  inj = inj' (unP $ (elemNo :: P t r))
  prj = prj' (unP $ (elemNo :: P t r))


{-# INLINE [2] decomp #-}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 v) = Right $ unsafeCoerce v
decomp (Union n v) = Left  $ Union (n-1) v


-- Specialized version
{-# RULES "decomp/singleton"  decomp = decomp0 #-}
{-# INLINE decomp0 #-}
decomp0 :: Union '[t] v -> Either (Union '[] v) (t v)
decomp0 (Union _ v) = Right $ unsafeCoerce v
-- No other case is possible

weaken :: Union r w -> Union (any ': r) w
weaken (Union n v) = Union (n+1) v

-- | Find an index of an element in a `list'
-- The element must exist
-- This is essentially a compile-time computation.
-- Using overlapping instances here is OK since this class is private to this
-- module
class FindElem (t :: * -> *) r where
  elemNo :: P t r

instance FindElem t (t ': r) where
  elemNo = P 0

instance FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + (unP $ (elemNo :: P t r))


-- | Using overlapping instances here is OK since this class is private to this
-- module
class EQU (a :: k) (b :: k) p | a b -> p
instance EQU a a 'True
instance (p ~ 'False) => EQU a b p

-- | This class is used for emulating monad transformers
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (EQU t1 t2 p, MemberU' p tag t1 (t2 ': r)) => MemberU2 tag t1 (t2 ': r)

class Member t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' 'True tag (tag e) (tag e ': r)
instance (Member t (t' ': r), MemberU2 tag t r) =>
           MemberU' 'False tag t (t' ': r)
