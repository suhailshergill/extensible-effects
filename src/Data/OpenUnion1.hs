{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- | Original work at http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs

-- Open unions (type-indexed co-products) for extensible effects
-- This implementation relies on _closed_ overlapping instances
-- (or closed type function overlapping soon to be added to GHC)

module Data.OpenUnion1( Union
                      , inj
                      , prj
                      , decomp
                      , Member
                      , MemberU
                      , MemberU2
                      , (:>)
                      , weaken
                      ) where

import Control.Applicative ((<$>))
import Data.Typeable

-- parameter r is phantom: it just tells what could be in the union
-- This encoding is quite like that in the HList paper.
-- The data constructor Union is not exported

data Union r v = forall t. (Functor t, Typeable1 t) => Union (t v)

-- for the sake of gcast1
newtype Id x = Id { runId :: x }

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (Union v) = Union (fmap f v)

infixl 4 <?>

(<?>) :: Maybe a -> a -> a
(Just x) <?> _ = x
_ <?> x = x

{-# INLINE inj #-}
inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
inj = Union

{-# INLINE prj #-}
prj :: (Functor t, Typeable1 t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) = runId <$> gcast1 (Id v)

{-# INLINE decomp #-}
decomp :: Typeable1 t => Union (t :> r) v -> Either (Union r v) (t v)
decomp (Union v) = Right . runId <$> gcast1 (Id v) <?> Left (Union v)

weaken :: (Typeable1 t, Functor t) => Union r w -> Union (t :> r) w
weaken (Union x) = Union x

class Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

-- A sum data type, for `composing' effects
-- In GHC 7.4, we should make it a list
-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

-- This class is used for emulating monad transformers
class Member t r => MemberU (tag :: * -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU tag (tag e) (tag e :> r)
instance MemberU tag t r => MemberU tag t (t' :> r)

-- A version of MemberU for argument of a different kind.
-- Latest GHC has well-functioning PolyKind extension; therefore,
-- MemberU2 can be merged with MemberU.
class Member t r => 
      MemberU2 (tag :: (* -> *) -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU2 tag (tag e) (tag e :> r)
instance MemberU2 tag t r => MemberU2 tag t (t' :> r)
