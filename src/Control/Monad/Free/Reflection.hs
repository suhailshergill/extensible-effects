{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
#endif

module Control.Monad.Free.Reflection(
  Free
  , freePure
  , freeImpure
  , freeMap
  , FreeView (..)
  , fromView
  , toView
  ) where

import Control.Arrow (Kleisli (..))
import Control.Applicative
import Control.Monad
import Data.TASequence
import Data.TASequence.FastCatQueue (FastTCQueue)

type TCQueue = FastTCQueue
type FC f = Kleisli (Free f)
type FMExp f a b = TCQueue (FC f) a b

data Free f a =
  forall x. FM (FreeView f x) (FMExp f x a)

freePure :: a -> Free f a
freePure = fromView . Pure

freeImpure :: f (Free f a) -> Free f a
freeImpure = fromView . Impure

-- | Case analysis for the 'Free' construction. Similar in spirit to 'either'
-- and 'maybe'.
freeMap :: Functor f
           => (a -> t) -- ^ function to be applied if value is Pure
           -> (f (Free f a) -> t) -- ^ function to be applied on Impure value
           -> Free f a -- ^ Free value to be mapped over
           -> t -- ^ result
freeMap f g mx = case toView mx of
  Pure x -> f x
  Impure u -> g u

instance Functor f => Functor (Free f) where
  fmap = liftM
instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap
instance Functor f => Monad (Free f) where
  return = freePure
  (FM m r) >>= f = FM m (r >< tsingleton (Kleisli f))

data FreeView f a = Pure a
                  | Impure (f (Free f a))

fromView :: FreeView f a -> Free f a
fromView x = FM x tempty

toView :: Functor f => Free f a -> FreeView f a
toView (FM h t) = case h of
  Pure x -> case tviewl t of
    TAEmptyL -> Pure x
    Kleisli hc :< tc -> toView (hc x >>>= tc)
  Impure f -> Impure (fmap (>>>= t) f)
  where
    (>>>=) :: Free f a -> FMExp f a b -> Free f b
    (FM x ys) >>>= r = FM x (ys >< r)
