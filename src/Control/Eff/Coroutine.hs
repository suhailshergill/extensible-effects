{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
-- | Coroutines implemented with extensible effects
module Control.Eff.Coroutine( Yield (..)
                            , yield
                            , runC
                            , Y (..)
                            ) where

import Control.Eff
import Control.Eff.Extend

-- ------------------------------------------------------------------------
-- | Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs
--
-- | The yield request: reporting a value of type e and suspending
-- the coroutine. Resuming with the value of type b
data Yield a b v where
  Yield :: a -> Yield a b b

-- | Yield a value of type a and suspend the coroutine.
yield :: (Member (Yield a b) r) => a -> Eff r b
yield x = send (Yield x)

-- | Status of a thread: done or reporting the value of the type a
--   (For simplicity, a co-routine reports a value but accepts unit)
--
--   Type parameter @r@ is the effect we're yielding from.
--
--   Type parameter @a@ is the type that is yielded.
--
--   Type parameter @w@ is the type of the value returned from the
--   coroutine when it has completed.
data Y r a w = Y a (w -> Eff r (Y r a w))
             | Done


-- | Launch a thread and report its status
runC :: Eff (Yield a b ': r) w -> Eff r (Y r a b)
runC m = handle_relay
  (const $ return Done)
  (\(Yield a) k -> return $ Y a k)
   m
