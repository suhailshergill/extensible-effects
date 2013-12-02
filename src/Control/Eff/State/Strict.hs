{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Strict state effect
--
-- Example: implementing `Control.Eff.Fresh`
--
-- > runFresh' :: (Typeable i, Enum i, Num i) => Eff (Fresh i :> r) w -> i -> Eff r w
-- > runFresh' m s = fst <$> runState s (loop $ admin m)
-- >  where
-- >   loop (Val x) = return x
-- >   loop (E u)   = case decomp u of
-- >     Right (Fresh k) -> do
-- >                       n <- get
-- >                       put (n + 1)
-- >                       loop (k n)
-- >     Left u' -> send (\k -> unsafeReUnion $ k <$> u') >>= loop
module Control.Eff.State.Strict( State
                               , get
                               , put
                               , modify
                               , runState
                               ) where

import Data.Typeable

import Control.Eff

-- | Strict state effect
data State s w = State (s -> s) (s -> w)
  deriving (Typeable, Functor)

-- | Write a new value of the state.
put :: (Typeable e, Member (State e) r) => e -> Eff r ()
put !s = modify $ const s

-- | Return the current value of the state.
get :: (Typeable e, Member (State e) r) => Eff r e
get = send (inj . State id)

-- | Transform the state with a function.
modify :: (Typeable s, Member (State s) r) => (s -> s) -> Eff r ()
modify f = send $ \k -> inj $ State f $ \_ -> k ()

-- | Run a State effect.
runState :: Typeable s
         => s                     -- ^ Initial state
         -> Eff (State s :> r) w  -- ^ Effect incorporating State
         -> Eff r (s, w)          -- ^ Effect containing final state and a return value
runState s0 = loop s0 . admin where
 loop !s (Val x) = return (s, x)
 loop !s (E u)   = handleRelay u (loop s) $
                       \(State t k) -> let s' = t s
                                       in loop s' (k s')
