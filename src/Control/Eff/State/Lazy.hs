{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Lazy state effect
module Control.Eff.State.Lazy( State (..)
                             , get
                             , put
                             , modify
                             , runState
                             , evalState
                             , execState
                             ) where

import Data.Typeable

import Control.Eff

-- | Strict state effect
data State s w = State (s -> s) (s -> w)
  deriving (Typeable, Functor)

-- | Write a new value of the state.
put :: (Typeable e, Member (State e) r) => e -> Eff r ()
put = modify . const

-- | Return the current value of the state.
get :: (Typeable e, Member (State e) r) => Eff r e
get = send . inj $ State id id

-- | Transform the state with a function.
modify :: (Typeable s, Member (State s) r) => (s -> s) -> Eff r ()
modify f = send . inj $ State f $ const ()

-- | Run a State effect.
runState :: Typeable s
         => s                     -- ^ Initial state
         -> Eff (State s :> r) w  -- ^ Effect incorporating State
         -> Eff r (s, w)          -- ^ Effect containing final state and a return value
runState = loop
  where
    loop s = freeMap
             (\x -> return (s, x))
             (\u -> handleRelay u (loop s) $
                    \(State t k) -> let s' = t s
                                    in loop s' (k s'))

-- | Run a State effect, discarding the final state.
evalState :: Typeable s => s -> Eff (State s :> r) w -> Eff r w
evalState s = fmap snd . runState s

-- | Run a State effect and return the final state.
execState :: Typeable s => s -> Eff (State s :> r) w -> Eff r s
execState s = fmap fst . runState s
