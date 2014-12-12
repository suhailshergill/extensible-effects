{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

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
                  , module Reflection
                  , Member
                  , SetMember
                  , Union
                  , (:>)
                  , inj
                  , prj
                  , prjForce
                  , decomp
                  , send
                  , run
                  , interpose
                  , handleRelay
                  , unsafeReUnion
                  ) where

import Control.Monad.Free.Reflection as Reflection
import Data.OpenUnion
import Data.Typeable
import Data.Void

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#endif

-- | Basic type returned by all computations with extensible effects. The @`Eff`
-- r@ type is a type synonym where the type @r@ is the type of effects that can
-- be handled, and the missing type @a@ (from the type application) is the type
-- of value that is returned.
--
-- Expressed another way: an `Eff` can either be a value (i.e., 'Pure' case), or
-- an effect of type @`Union` r@ producing another `Eff` (i.e., 'Impure'
-- case). The result is that an `Eff` can produce an arbitrarily long chain of
-- @`Union` r@ effects, terminated with a pure value.
--
-- As is made explicit below, the `Eff` type is simply the Free monad resulting from the
-- @`Union` r@ functor.
--
--     @type `Eff` r a = `Free` (`Union` r) a@
type Eff r = Free (Union r)

-- | Given a method of turning requests into results,
-- we produce an effectful computation.
send :: Union r a -> Eff r a
send = freeImpure . (fmap freePure)
{-# INLINE send #-}

-- | Get the result from a pure computation.
run :: Eff Void w -> w
run = freeMap id
      (\_ -> error "extensible-effects: the impossible happened!")
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
  where passOn u' = send u' >>= loop
{-# INLINE handleRelay #-}

-- | Given a request, either handle it or relay it. Both the handler
-- and the relay can produce the same type of request that was handled.
interpose :: (Typeable1 t, Functor t, Member t r)
          => Union r v
          -> (v -> Eff r a)
          -> (t v -> Eff r a)
          -> Eff r a
interpose u loop h = maybe (send u >>= loop) h $ prj u
{-# INLINE interpose #-}
