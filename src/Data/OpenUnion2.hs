{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- Open unions (type-indexed co-products) for extensible effects
-- This implementation relies on _closed_ type families added
-- to GHC 7.8
-- It has NO overlapping instances

module OpenUnion2 (Union, inj, prj, decomp, 
                   Member, MemberU2, (:>), weaken
                  ) where

import Data.Typeable

-- parameter r is phantom: it just tells what could be in the union
-- This encoding is quite like that in the HList paper.
-- The data constructor Union is not exported

data Union r v where                      -- r is of a kind [*->*]
  Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

newtype Id x = Id x                     -- for the sake of gcast1

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (Union (Id v)) = Union (Id (fmap f v))

{-# INLINE inj #-}
inj :: (Functor t, Typeable t, Member t r) => t v -> Union r v
inj x = Union (Id x)

{-# INLINE prj #-}
prj :: (Functor t, Typeable t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) | Just (Id x) <- gcast1 v = Just x
prj _ = Nothing

{-# INLINE decomp #-}
decomp :: Typeable t => Union (t :> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v) = Left (Union v)

weaken :: (Typeable t, Functor t) => Union r w -> Union (t :> r) w
weaken (Union x) = Union x

-- Class Member is defined only for the sake of the interface
-- compatibility with OpenUnion1
-- Generally, the closed type family Member' below could be
-- used instead

class (Member' t r ~ True) => Member (t :: * -> *) r
instance (Member' t r ~ True) => Member t r

type family Member' (t :: * -> *) r :: Bool where
  Member' t (t :> r)  = True
  Member' t ()        = False
  Member' t (t' :> r) = Member' t r


-- A sum data type, for `composing' effects
-- In GHC 7.4, we should make it a list
-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = True
  EQU a b = False
  
-- This class is used for emulating monad transformers
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 :> r)) => MemberU2 tag t1 (t2 :> r)

class Member t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' True tag (tag e) (tag e :> r)
instance (Member' t (t' :> r) ~ True, MemberU2 tag t r) =>
           MemberU' False tag t (t' :> r)
