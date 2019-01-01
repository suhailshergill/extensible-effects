{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- | Strict state effect
module Control.Eff.State.Strict where

import Control.Eff
import Control.Eff.Extend

import Control.Eff.Writer.Strict
import Control.Eff.Reader.Strict

import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.Function (fix)

-- ------------------------------------------------------------------------
-- | State, strict
--
-- Initial design:
-- The state request carries with it the state mutator function
-- We can use this request both for mutating and getting the state.
-- But see below for a better design!
--
-- > data State s v where
-- >   State :: (s->s) -> State s s
--
-- In this old design, we have assumed that the dominant operation is
-- modify. Perhaps this is not wise. Often, the reader is most nominant.
--
-- See also below, for decomposing the State into Reader and Writer!
--
-- The conventional design of State
data State s v where
  Get :: State s s
  Put :: !s -> State s ()

-- | Embed a pure value in a stateful computation, i.e., given an
-- initial state, how to interpret a pure value in a stateful
-- computation.
withState :: Monad m => a -> s -> m (a, s)
withState x s = return (x, s)

-- | Handle 'State s' requests
instance Handle (State s) r a (s -> k) where
  handle step q sreq s = case sreq of
    Get    -> step (q ^$ s) s
    Put s' -> step (q ^$ ()) s'

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (State s ': r)) where
    type StM (Eff (State s ': r)) a = StM (Eff r) (a,s)
    liftBaseWith f = do s <- get
                        raise $ liftBaseWith $ \runInBase ->
                          f (runInBase . runState s)
    restoreM x = do !(a, s :: s) <- raise (restoreM x)
                    put s
                    return a


-- | Return the current value of the state. The signatures are inferred
{-# NOINLINE get #-}
get :: Member (State s) r => Eff r s
get = send Get
{-# RULES
  "get/bind" forall k. get >>= k = send Get >>= k
 #-}

-- | Write a new value of the state.
{-# NOINLINE put #-}
put :: Member (State s) r => s -> Eff r ()
put !s = send (Put s)
{-# RULES
  "put/bind"     forall k v. put v >>= k = send (Put v) >>= k
 #-}
{-# RULES
  "put/semibind" forall k v. put v >>  k = send (Put v) >>= (\() -> k)
 #-}
-- The purpose of the rules is to expose send, which is then being
-- fuzed by the send/bind rule. The send/bind rule is very profitable!
-- These rules are essentially inlining of get/put. Somehow GHC does not
-- inline get/put, even if I put the INLINE directives and play with phases.
-- (Inlining works if I use 'inline' explicitly).

-- | Run a State effect
runState :: s                     -- ^ Initial state
         -> Eff (State s ': r) a  -- ^ Effect incorporating State
         -> Eff r (a, s)          -- ^ Effect containing final state and a return value
runState !s m = fix (handle_relay withState) m s

-- | Transform the state with a function.
modify :: (Member (State s) r) => (s -> s) -> Eff r ()
modify f = get >>= put . f

-- | Run a State effect, discarding the final state.
evalState :: s -> Eff (State s ': r) a -> Eff r a
evalState !s = fmap fst . runState s
{-# INLINE evalState #-}

-- | Run a State effect and return the final state.
execState :: s -> Eff (State s ': r) a -> Eff r s
execState !s = fmap snd . runState s
{-# INLINE execState #-}

-- | An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
data TxState s = TxState

-- | Embed Transactional semantics to a stateful computation.
withTxState :: Member (State s) r => a -> s -> Eff r a
withTxState x s = put s >> return x

-- | Confer transactional semantics on a stateful computation.
transactionState :: forall s r a. Member (State s) r
                 => TxState s -> Eff r a -> Eff r a
transactionState _ m = do
  s <- get
  (fix $ respond_relay @(State s) (withTxState @s)) m s

-- | A different representation of State: decomposing State into mutation
-- (Writer) and Reading. We don't define any new effects: we just handle the
-- existing ones.  Thus we define a handler for two effects together.
runStateR :: s -> Eff (Writer s ': Reader s ': r) a -> Eff r (a, s)
runStateR !s m = loop m s
 where
   loop :: Eff (Writer s ': Reader s ': r) a -> s -> Eff r (a, s)
   loop (Val x) = withState x
   loop (E q u) = case u of
     U0 (Tell w) -> handle loop q (Put w)
     U1 (U0 Ask) -> handle loop q Get
     U1 (U1 u') -> relay (qComp q loop) u'
