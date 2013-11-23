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
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Original work available at <http://okmij.org/ftp/Hgetell/extensible/Eff.hs>.
-- This module implements extensible effects as an alternative to monad transformers,
-- as described in <http://okmij.org/ftp/Hgetell/extensible/exteff.pdf>.
--
-- Extensible Effects are implemented as typeclass constraints on an Eff[ect] datatype.
-- A contrived example is:
--
-- >   {-# LANGUAGE FlexibleContexts #-}
-- >   import Control.Eff
-- >   import Control.Monad (void)
-- >
-- >   -- Print a list of numbers, then print their sum.
-- >   printAndSum :: (Member (Lift IO) e, Member (State Integer) e) => [Integer] -> Eff e ()
-- >   printAndSum (x:xs) = do
-- >        lift $ putStrLn $ show x
-- >        onState (+ x)
-- >        printAndSum xs
-- >   printAndSum [] = getState >>= lift . putStrLn . (show :: Integer -> String)
-- >
-- >   -- printAndSum [1..5]
-- >   main :: IO ()
-- >   main = void $ runLift $ runState (0 :: Integer) $ printAndSum [1..5]
module Control.Eff( Eff
                  , VE (..)
                  , Member
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
                  ) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Monad (ap)
import Data.OpenUnion1
import Data.Typeable

-- | A `VE` is either a value, or an effect of type `Union r` producing another `VE`.
-- The result is that a `VE` can produce an arbitrarily long chain of `Union r`
-- effects, terminated with a pure value.
data VE w r = Val w | E !(Union r (VE w r))

fromVal :: VE w r -> w
fromVal (Val w) = w
fromVal _ = error "fromVal E"

-- | Basic datatype returned by all computations with extensible effects.
-- The type `r` is the type of effects that can be handled,
-- and `a` is the type of value that is returned.
newtype Eff r a = Eff { runEff :: forall w. (a -> VE w r) -> VE w r }

instance Functor (Eff r) where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Applicative (Eff r) where
    pure = return
    (<*>) = ap

instance Monad (Eff r) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- | Given a method of turning requests into results,
-- we produce an effectful computation.
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff (E . f)

-- | Tell an effectful computation that you're ready to start running effects
-- and return a value.
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

-- ------------------------------------------------------------------------
-- The initial case, no effects

-- | Get the result from a pure computation.
run :: Eff () w -> w
run = fromVal . admin
-- the other case is unreachable since () has no constructors
-- Therefore, run is a total function if m Val terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handleRelay :: Typeable1 t =>
     Union (t :> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handleRelay u loop h = either passOn h $ decomp u
  where passOn u' = send (<$> u') >>= loop
  -- perhaps more efficient:
  -- passOn u' = send (\k -> fmap (\w -> runEff (loop w) k) u')

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

interpose :: (Typeable1 t, Functor t, Member t r) =>
     Union r v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
interpose u loop h = maybe (send (<$> u) >>= loop) h $ prj u
