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
-- | Strict state effect
module Control.Eff.State.Strict where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Lift

import Control.Eff.Writer.Strict
import Control.Eff.Reader.Strict

import Control.Monad.Base
import Control.Monad.Trans.Control

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

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
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

runState' :: s -> Eff (State s ': r) a -> Eff r (a, s)
runState' !s =
  handle_relay_s s (\s0 x -> return (x,s0))
                   (\s0 sreq k -> case sreq of
                       Get    -> k s0 s0
                       Put s1 -> k s1 ())

-- Since State is so frequently used, we optimize it a bit
-- | Run a State effect
runState :: s                     -- ^ Effect incorporating State
         -> Eff (State s ': r) a  -- ^ Initial state
         -> Eff r (a, s)          -- ^ Effect containing final state and a return value
runState !s (Val x) = return (x,s)
runState !s (E u q) = case decomp u of
  Right Get     -> runState s (q ^$ s)
  Right (Put s1) -> runState  s1 (q ^$ ())
  Left  u1 -> E u1 (singleK (\x -> runState s (q ^$ x)))

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
transactionState :: forall s r a. Member (State s) r =>
                    TxState s -> Eff r a -> Eff r a
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r a -> Eff r a
   loop s (Val x) = put s >> return x
   loop s (E (u::Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (q ^$ s)
     Just (Put s') -> loop s'(q ^$ ())
     _             -> E u (qComps q (loop s))

-- | A different representation of State: decomposing State into mutation
-- (Writer) and Reading. We don't define any new effects: we just handle the
-- existing ones.  Thus we define a handler for two effects together.
runStateR :: s -> Eff (Writer s ': Reader s ': r) a -> Eff r (a, s)
runStateR !s m = loop s m
 where
   loop :: s -> Eff (Writer s ': Reader s ': r) a -> Eff r (a, s)
   loop s0 (Val x) = return (x,s0)
   loop s0 (E u q) = case decomp u of
     Right (Tell w) -> k w ()
     Left  u1  -> case decomp u1 of
       Right Ask -> k s0 s0
       Left u2 -> E u2 (singleK (k s0))
    where k x = qComp q (loop x)
