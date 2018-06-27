{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -Wwarn #-}

{-# LANGUAGE CPP #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ < 710 || FORCE_OU51
{-# LANGUAGE OverlappingInstances #-}
#endif

-- Only for SetMember below, when emulating Monad Transformers
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
--
-- The interface is the same as of other OpenUnion*.hs
module Data.OpenUnion ( Union
                      , inj
                      , prj
                      , decomp
                      , Member
                      , SetMember
                      , type(<::)
                      , weaken
                      ) where

import Unsafe.Coerce(unsafeCoerce)

#if __GLASGOW_HASKELL__ > 800
import Data.Kind (Constraint)
import GHC.TypeLits
#else
import GHC.Exts (Constraint)
#endif

-- | The data constructors of Union are not exported
--
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

-- | Typeclass that asserts that effect @t@ is contained inside the effect-list
-- @r@.
--
-- The @FindElem@ typeclass is necessary for implementation reasons and is not
-- required for using the effect list.
class (FindElem t r) => Member (t :: * -> *) r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

#if __GLASGOW_HASKELL__ < 710 || FORCE_OU51
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
#else
-- | Explicit type-level equality condition is a dirty
-- hack to eliminate the type annotation in the trivial case,
-- such as @run (runReader () get)@.
--
-- There is no ambiguity when finding instances for
-- @Member t (a ': b ': r)@, which the second instance is selected.
--
-- The only case we have to concerned about is  @Member t '[s]@.
-- But, in this case, values of definition is the same (if present),
-- and the first one is chosen according to GHC User Manual, since
-- the latter one is incoherent. This is the optimal choice.
instance {-# OVERLAPPING #-} t ~ s => Member t '[s] where
   {-# INLINE inj #-}
   {-# INLINE prj #-}
   inj x           = Union 0 x
   prj (Union _ x) = Just (unsafeCoerce x)
-- Note that if it weren't for us wanting to use the specialized instance above
-- we wouldn't need the INCOHERENT pragma below
-- TODO: consider impact of disabling specialization
instance {-# INCOHERENT #-}  (FindElem t r) => Member t r where
  {-# INLINE inj #-}
  {-# INLINE prj #-}
  inj = inj' (unP $ (elemNo :: P t r))
  prj = prj' (unP $ (elemNo :: P t r))
#endif

-- | A useful operator for reducing boilerplate.
--
-- @
-- f :: [Reader Int, Writer String] <:: r
--   => a -> Eff r b
-- @
-- is equal to
--
-- @
-- f :: (Member (Reader Int) r, Member (Writer String) r)
--   => a -> Eff r b
-- @
type family (<::) (ms :: [* -> *]) r where
  (<::) '[] r = (() :: Constraint)
  (<::) (m ': ms) r = (Member m r, (<::) ms r)

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
#if __GLASGOW_HASKELL__ < 710 || FORCE_OU51
instance FindElem t r => FindElem t (t' ': r) where
#else
instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
#endif
  elemNo = P $ 1 + (unP $ (elemNo :: P t r))
#if __GLASGOW_HASKELL__ > 800
instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType t ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem t '[] where
  elemNo = error "unreachable"
#endif

-- | Using overlapping instances here is OK since this class is private to this
-- module
class EQU (a :: k) (b :: k) p | a b -> p
instance EQU a a 'True
#if __GLASGOW_HASKELL__ < 710 || FORCE_OU51
instance (p ~ 'False) => EQU a b p
#else
instance {-# OVERLAPPABLE #-} (p ~ 'False) => EQU a b p
#endif

-- | This class is used for emulating monad transformers
class Member t r => SetMember (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (EQU t1 t2 p, MemberU' p tag t1 (t2 ': r)) => SetMember tag t1 (t2 ': r)

class Member t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' 'True tag (tag e) (tag e ': r)
instance (Member t (t' ': r), SetMember tag t r) =>
           MemberU' 'False tag t (t' ': r)
