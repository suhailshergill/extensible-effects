{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Coroutines implemented with extensible effects
module Control.Eff.Coroutine( Yield
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
-- (For simplicity, a co-routine reports a value but accepts unit)
data Y r a = Done | Y a (() -> Eff r (Y r a))

-- | Launch a thread and report its status.
runC :: Typeable a => Eff (Yield a :> r) w -> Eff r (Y r a)
runC m = loop (admin m)
  where
    loop (Val _) = return Done
    loop (E u)   = handleRelay u loop $
                    \(Yield x k) -> return (Y x (loop . k))
