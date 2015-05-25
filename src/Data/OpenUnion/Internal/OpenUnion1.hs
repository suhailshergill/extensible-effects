{-# OPTIONS_HADDOCK hide, show-extensions #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Safe #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}

-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
-- This implementation relies on _closed_ overlapping instances
-- (or closed type function overlapping soon to be added to GHC).
module Data.OpenUnion.Internal.OpenUnion1( OU1
                                         , module Base
                                         ) where

import Data.OpenUnion.Internal.Base as Base

-- | Label to represent 'OpenUnion1' implementation
data OU1

-- | @`MemberImpl` OU1 t r@ specifies whether @t@ is present anywhere in the
-- sum type @r@, where @t@ is some effectful type
type instance MemberConstraint OU1 t r = ()
instance MemberImpl OU1 t (t :> r)
instance MemberImpl OU1 t r => MemberImpl OU1 t (t' :> r)

-- | For emulating monad transformers
type instance MemberUConstraint OU1 t r = (MemberImpl OU1 t r)
instance MemberUImpl OU1 tag (tag e) (tag e :> r)
instance MemberUImpl OU1 tag t r => MemberUImpl OU1 tag t (t' :> r)
