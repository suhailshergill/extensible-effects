{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Coroutines implemented with extensible effects
module Control.Eff.Coroutine( Yield (..)
                            , yield
                            , runC
                            , Y (..)
                            ) where

import Data.Typeable

import Control.Eff

-- | The yield request: reporting a value of type e and suspending
-- the coroutine. For readability, a coroutine accepts a unit to produce
-- its value.
data Yield a v = Yield a (() -> v)
    deriving (Typeable, Functor)

-- | Yield a value of type a and suspend the coroutine.
yield :: (Typeable a, Member (Yield a) r) => a -> Eff r ()
yield x = send (inj . Yield x)

-- | Status of a thread: done or reporting the value of the type a
--   (For simplicity, a co-routine reports a value but accepts unit)
--
--   Type parameter @r@ is the effect we're yielding from.
--
--   Type parameter @a@ is the type that is yielded.
--
--   Type parameter @w@ is the type of the value returned from the
--   coroutine when it has completed.
data Y r a w = Y a (() -> Eff r (Y r a w))
             | Done w

-- | Launch a thread and report its status.
runC :: Typeable a => Eff (Yield a :> r) w -> Eff r (Y r a w)
runC m = loop (admin m)
  where
    loop (Pure x) = return (Done x)
    loop (Free u)   = handleRelay u loop $
                    \(Yield x k) -> return (Y x (loop . k))
