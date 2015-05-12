{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
{-# LANGUAGE Safe #-}

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
import qualified Data.OpenUnion.Imports as P
import Control.Monad
import Data.TASequence
import Data.TASequence.FastCatQueue (FastTCQueue)

-- | Specific type-aligned sequence used to store chain of monadic binds.
type TCQ = FastTCQueue

-- | Type used to denote monadic chain of binds. As expected this builds on the
-- Kleisli representation.
type FreeExp f a b = TCQ (Kleisli (Free f)) a b

-- | The abstract Free datatype. Original work available at
-- <http://okmij.org/ftp/Haskell/AlgorithmsH1.html#reflection-without-remorse>.
data Free f a =
  forall x. Free (FreeView f x) (FreeExp f x a)

-- | Inject a pure value into Free
freePure :: a -> Free f a
freePure = fromView . Pure

-- | Inject an impure value into Free
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
instance Functor f => P.Applicative (Free f) where
  pure = return
  (<*>) = ap
instance Functor f => Monad (Free f) where
  return = freePure
  mx >>= f = mx ^>>= tsingleton (Kleisli f)

-- | The traditional 'view' of Free constructions
data FreeView f a = Pure a -- ^ case embedding pure values
                  | Impure (f (Free f a)) -- ^ case embedding impure values
                                          -- nested in @f@. Traditionally this
                                          -- is the @Control.Monad.Free.Free@
                                          -- constructor, but that's confusing.

-- | A way to get a 'Free' construction from the view by constructing an
-- explicit expression with one element.
fromView :: FreeView f a -> Free f a
fromView x = Free x tempty

-- | A way to evaluate the 'Free' construction to its view (i.e., head normal
-- form). This includes the logic to perform one level of monadic bind as needed
-- from the 'FreeExp' representation.
toView :: Functor f => Free f a -> FreeView f a
toView (Free h t) = case h of
  Pure x -> case tviewl t of
    TAEmptyL -> Pure x
    hc :< tc -> toView (runKleisli hc x ^>>= tc)
  Impure f -> Impure (fmap (^>>= t) f)

-- | The essence of monadic '>>=', i.e., append/concatenation (of sorts)
(^>>=) :: Free f a -> FreeExp f a b -> Free f b
(Free x ys) ^>>= r = Free x (ys >< r)
