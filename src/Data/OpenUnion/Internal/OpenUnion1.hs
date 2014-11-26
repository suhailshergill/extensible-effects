{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Only for MemberU2 below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, OverlappingInstances, UndecidableInstances #-}

-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
-- This implementation relies on _closed_ overlapping instances
-- (or closed type function overlapping soon to be added to GHC).
module Data.OpenUnion.Internal.OpenUnion1( Union (..)
                                         , (:>)
                                         , Member
                                         , MemberU2
                                         ) where

import Data.OpenUnion.Internal.Base

-- | The @`Member` t r@ specifies whether @t@ is present anywhere in the sum
-- type @r@, where @t@ is some effectful type,
-- e.g. @`Lift` `IO`@, @`State` Int`@.
class Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

-- | This class is used for emulating monad transformers
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU2 tag (tag e) (tag e :> r)
instance MemberU2 tag t r => MemberU2 tag t (t' :> r)
