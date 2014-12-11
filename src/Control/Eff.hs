{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
-- needed for the orphan Typeable instance for Codensity below
{-# OPTIONS -fno-warn-orphans #-}
#endif

-- | Original work available at <http://okmij.org/ftp/Haskell/extensible/Eff.hs>.
-- This module implements extensible effects as an alternative to monad transformers,
-- as described in <http://okmij.org/ftp/Haskell/extensible/exteff.pdf>.
--
-- Extensible Effects are implemented as typeclass constraints on an Eff[ect] datatype.
-- A contrived example is:
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- > import Control.Eff
-- > import Control.Eff.Lift
-- > import Control.Eff.State
-- > import Control.Monad (void)
-- > import Data.Typeable
-- >
-- > -- Write the elements of a list of numbers, in order.
-- > writeAll :: (Typeable a, Member (Writer a) e)
-- >          => [a]
-- >          -> Eff e ()
-- > writeAll = mapM_ putWriter
-- >
-- > -- Add a list of numbers to the current state.
-- > sumAll :: (Typeable a, Num a, Member (State a) e)
-- >        => [a]
-- >        -> Eff e ()
-- > sumAll = mapM_ (onState . (+))
-- >
-- > -- Write a list of numbers and add them to the current state.
-- > writeAndAdd :: (Member (Writer Integer) e, Member (State Integer) e)
-- >             => [Integer]
-- >             -> Eff e ()
-- > writeAndAdd l = do
-- >     writeAll l
-- >     sumAll l
-- >
-- > -- Sum a list of numbers.
-- > sumEff :: (Num a, Typeable a) => [a] -> a
-- > sumEff l = let (s, ()) = run $ runState 0 $ sumAll l
-- >            in s
-- >
-- > -- Safely get the last element of a list.
-- > -- Nothing for empty lists; Just the last element otherwise.
-- > lastEff :: Typeable a => [a] -> Maybe a
-- > lastEff l = let (a, ()) = run $ runWriter $ writeAll l
-- >             in a
-- >
-- > -- Get the last element and sum of a list
-- > lastAndSum :: (Typeable a, Num a) => [a] -> (Maybe a, a)
-- > lastAndSum l = let (lst, (total, ())) = run $ runWriter $ runState 0 $ writeAndAdd l
-- >                in (lst, total)
module Control.Eff(
                    Eff
                  , VE
                  , Free (..)
                  , Member
                  , SetMember
                  , Union
                  , (:>)
                  , inj
                  , prj
                  , prjForce
                  , decomp
                  , send
                  , admin
                  , run
                  , interpose
                  , handleRelay
                  , unsafeReUnion
                  ) where

import Control.Applicative ((<$>))
import Control.Monad.Codensity (Codensity (..))
import Control.Monad.Free (Free (..))
import Data.OpenUnion
import Data.Typeable
import Data.Void

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#endif

-- | A `VE` is either a value, or an effect of type @`Union` r@ producing another `VE`.
-- The result is that a `VE` can produce an arbitrarily long chain of @`Union` r@
-- effects, terminated with a pure value.
--
-- As is made explicit here, `VE` is simply the Free monad resulting from the
-- @`Union` r@ functor.
type VE r = Free (Union r)

-- | Basic datatype returned by all computations with extensible effects. The
-- @`Eff` r@ type is a type synonym where the type @r@ is the type of effects
-- that can be handled, and the missing type @a@ (from the type application) is
-- the type of value that is returned.
--
-- As is made explicit below, the `Eff` type is simply the application of the
-- Codensity transformer to `VE`:
--
--     @type `Eff` r a = `Codensity` (`VE` r) a@
--
-- This is done to gain the asymptotic speedups for scenarios where there is a
-- single 'execution' stage where the built up monadic computation gets
-- executed. For scenarios where the computation execution and building stages
-- are interspersed, the reflection without remorse techniques would be a
-- better fit. See <https://github.com/atzeus/reflectionwithoutremorse>.
type Eff r = Codensity (VE r)
#if __GLASGOW_HASKELL__ >= 708
-- as of version 4.1.0.1 Codensity in kan-extensions does not have a a Typeable
-- instance. it doesn't seem possible to be able to define an orphan Typeable
-- instance for Codensity in ghc-7.6
deriving instance Typeable Codensity
#endif

-- | Given a method of turning requests into results,
-- we produce an effectful computation.
send :: (forall w. (a -> VE r w) -> Union r (VE r w)) -> Eff r a
send f = Codensity (Free . f)
{-# INLINE send #-}

-- | Tell an effectful computation that you're ready to start running effects
-- and return a value.
admin :: Eff r w -> VE r w
admin (Codensity m) = m Pure
{-# INLINE admin #-}

-- | Get the result from a pure computation.
run :: Eff Void w -> w
run x = case admin x of
  Pure w -> w
  _ -> error $
       "extensible-effects: the impossible happened! Void has no constructors."
{-# INLINE run #-}
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if m Val terminates.

-- | Given a request, either handle it or relay it.
handleRelay :: Typeable1 t
            => Union (t :> r) v -- ^ Request
            -> (v -> Eff r a)   -- ^ Relay the request
            -> (t v -> Eff r a) -- ^ Handle the request of type t
            -> Eff r a
handleRelay u loop h = either passOn h $ decomp u
  where passOn u' = send (<$> u') >>= loop
  -- perhaps more efficient:
  -- passOn u' = send (\k -> fmap (\w -> runCodensity (loop w) k) u')
{-# INLINE handleRelay #-}

-- | Given a request, either handle it or relay it. Both the handler
-- and the relay can produce the same type of request that was handled.
interpose :: (Typeable1 t, Functor t, Member t r)
          => Union r v
          -> (v -> Eff r a)
          -> (t v -> Eff r a)
          -> Eff r a
interpose u loop h = maybe (send (<$> u) >>= loop) h $ prj u
{-# INLINE interpose #-}
