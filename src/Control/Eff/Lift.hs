{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 708
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
-- | Lifting primitive Monad types to effectful computations.
-- We only allow a single Lifted Monad because Monads aren't commutative
-- (e.g. Maybe (IO a) is functionally distinct from IO (Maybe a)).
module Control.Eff.Lift ( Lift (..)
                       , lift
                       , runLift
                       ) where

import Control.Eff
import Data.OpenUnion

-- ------------------------------------------------------------------------
-- | Lifting: emulating monad transformers
newtype Lift m a = Lift (m a)

-- | We make the Lift layer to be unique, using MemberU2
lift :: (MemberU2 Lift (Lift m) r) => m a -> Eff r a
lift = send . Lift

-- | The handler of Lift requests. It is meant to be terminal:
-- we only allow a single Lifted Monad.
runLift :: Monad m => Eff '[Lift m] w -> m w
runLift (Val x) = return x
runLift (E u q) = case prj u of
                  Just (Lift m) -> m >>= runLift . qApp q
                  Nothing -> error "Impossible: Nothing cannot occur"
