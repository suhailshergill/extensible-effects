{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- | Lazy state effect
module Control.Eff.State.Lazy where

import Control.Eff
import Control.Eff.Extend

import Control.Eff.Writer.Lazy
import Control.Eff.Reader.Lazy

import Control.Monad.Base
import Control.Monad.Trans.Control

-- ------------------------------------------------------------------------
-- | State, lazy
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
  Put :: s -> State s ()

-- | Embed a pure value in a stateful computation, i.e., given an
-- initial state, how to interpret a pure value in a stateful
-- computation.
withState :: Monad m => a -> s -> m (a, s)
withState x s = return (x, s)

-- | Handle 'State s' requests
instance Handle (State s) (s -> r) where
  handle k sreq s = case sreq of
    Get    -> k s s
    Put s' -> k () s'

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (State s ': r)) where
    type StM (Eff (State s ': r)) a = StM (Eff r) (a,s)
    liftBaseWith f = do s <- get
                        raise $ liftBaseWith $ \runInBase ->
                          f (runInBase . runState s)
    restoreM x = do (a, s :: s) <- raise (restoreM x)
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
put s = send (Put s)
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

-- | Run a state effect. compared to the @runState@ function, this is
--   implemented naively and is expected to perform slower.
runState' :: s -> Eff (State s ': r) a -> Eff r (a, s)
runState' s m = handle_relay withState m s

-- | Run a State effect. This variant is a bit optimized compared to
--   @runState'@.
runState :: s                     -- ^ Initial state
         -> Eff (State s ': r) a  -- ^ Effect incorporating State
         -> Eff r (a, s)          -- ^ Effect containing final state and a return value
runState s (Val x) = return (x,s)
runState s (E q u) = case decomp u of
  Right Get     -> runState s (q ^$ s)
  Right (Put s1) -> runState s1 (q ^$ ())
  Left  u1 -> E (singleK (\x -> runState s (q ^$ x))) u1

-- | Transform the state with a function.
modify :: (Member (State s) r) => (s -> s) -> Eff r ()
modify f = get >>= put . f

-- | Run a State effect, discarding the final state.
evalState :: s -> Eff (State s ': r) a -> Eff r a
evalState s = fmap fst . runState s

-- | Run a State effect and return the final state.
execState :: s -> Eff (State s ': r) a -> Eff r s
execState s = fmap snd . runState s

-- | An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
data TxState s v where
  TxState :: TxState s s
type TxStateT s = TxState s s

-- | Embed Transactional semantics to a stateful computation.
withTxState :: Member (State s) r => a -> s -> Eff r a
withTxState x s = put s >> return x

-- | Confer transactional semantics on a stateful computation.
transactionState' :: forall s r a. Member (State s) r
                  => TxStateT s -> Eff r a -> Eff r a
transactionState' _ m = do
  s <- get
  (interpose' @(State s) (withTxState @s)) m s

-- | More involved implementation.
transactionState :: forall s r a. Member (State s) r
                 => TxStateT s -> Eff r a -> Eff r a
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r a -> Eff r a
   loop s (Val x) = withTxState x s
   loop s (E q (u::Union r b)) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (q ^$ s)
     Just (Put s') -> loop s'(q ^$ ())
     _             -> E (qComps q (loop s)) u

-- | A different representation of State: decomposing State into mutation
-- (Writer) and Reading. We don't define any new effects: we just handle the
-- existing ones.  Thus we define a handler for two effects together.
runStateR :: s -> Eff (Writer s ': Reader s ': r) a -> Eff r (a, s)
runStateR s m = loop s m
 where
   loop :: s -> Eff (Writer s ': Reader s ': r) a -> Eff r (a, s)
   loop s0 (Val x) = x `withState` s0
   loop s0 (E q u) = case decomp u of
     Right (Tell w) -> handle k (Put w) s0
     Left  u1  -> case decomp u1 of
       Right Ask -> handle k Ask s0
       Left u2 -> relay k u2 s0
    where k s' x = qComp q (loop x) s'
