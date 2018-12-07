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
-- | Lazy state effect
module Control.Eff.State.OnDemand where

import Control.Eff
import Control.Eff.Extend

import Control.Eff.Writer.Lazy
import Control.Eff.Reader.Lazy
import qualified Control.Eff.State.Lazy as S

import Control.Monad.Base
import Control.Monad.Trans.Control

-- ------------------------------------------------------------------------
-- | State, lazy (i.e., on-demand)
--
-- Extensible effects make it clear that where the computation is delayed
-- (which I take as an advantage) and they do maintain the degree of
-- extensibility (the delayed computation must be effect-closed, but the
-- whole computation does not have to be).
data OnDemandState s v where
  Get  :: OnDemandState s s
  Put  :: s -> OnDemandState s ()
  Delay :: Eff '[OnDemandState s] a  -> OnDemandState s a --  Eff as a transformer

-- | Given a continuation, respond to requests
instance Handle (OnDemandState s) (s -> r) where
  handle k Get s = k s s
  handle k (Put s) _ = k () s
  handle k (Delay m) s = let ~(x, s') = run $ handle_relay S.withState m s
                         in k x s'

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (OnDemandState s ': r)) where
    type StM (Eff (OnDemandState s ': r)) a = StM (Eff r) (a,s)
    liftBaseWith f = do s <- get
                        raise $ liftBaseWith $ \runInBase ->
                          f (runInBase . runState s)
    restoreM x = do (a, s :: s) <- raise (restoreM x)
                    put s
                    return a


-- | Return the current value of the state. The signatures are inferred
{-# NOINLINE get #-}
get :: Member (OnDemandState s) r => Eff r s
get = send Get
{-# RULES
  "get/bind" forall k. get >>= k = send Get >>= k
 #-}

-- | Write a new value of the state.
{-# NOINLINE put #-}
put :: Member (OnDemandState s) r => s -> Eff r ()
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

onDemand :: Member (OnDemandState s) r => Eff '[OnDemandState s] v -> Eff r v
onDemand = send . Delay

runState' :: s -> Eff (OnDemandState s ': r) w -> Eff r (w,s)
runState' s m = handle_relay S.withState m s

-- Since State is so frequently used, we optimize it a bit
-- | Run a State effect
runState :: s                            -- ^ Initial state
         -> Eff (OnDemandState s ': r) w -- ^ Effect incorporating State
         -> Eff r (w,s)                  -- ^ Effect containing final state and a return value
runState s (Val x) = S.withState x s
runState s0 (E q u0) = case decomp u0 of
  Right Get     -> runState s0 (q ^$ s0)
  Right (Put s1) -> runState s1 (q ^$ ())
  Right (Delay m1) -> let ~(x,s1) = run $ runState s0 m1
                      in runState s1 (q ^$ x)
  Left  u -> E (singleK (\x -> runState s0 (q ^$ x))) u

-- | Transform the state with a function.
modify :: (Member (OnDemandState s) r) => (s -> s) -> Eff r ()
modify f = get >>= put . f

-- | Run a State effect, discarding the final state.
evalState :: s -> Eff (OnDemandState s ': r) w -> Eff r w
evalState s = fmap fst . runState s

-- | Run a State effect and return the final state.
execState :: s -> Eff (OnDemandState s ': r) w -> Eff r s
execState s = fmap snd . runState s

-- | A different representation of State: decomposing State into mutation
-- (Writer) and Reading. We don't define any new effects: we just handle the
-- existing ones.  Thus we define a handler for two effects together.
runStateR :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w,s)
runStateR s (Val x) = S.withState x s
runStateR s (E q u) = case u of
  U0 (Tell w) -> handle k (S.Put w) s
  U1 (U0 Ask) -> handle k S.Get s
  U1 (U1 u') -> relay k u' s
  where k = qComp q (flip runStateR)

-- | Backwards state
-- The overall state is represented with two attributes: the inherited
-- getAttr and the synthesized putAttr.
-- At the root node, putAttr becomes getAttr, tying the knot.
-- As usual, the inherited attribute is the argument (i.e., the `environment')
-- and the synthesized is the result of the handler |go| below.
runStateBack0 :: Eff '[OnDemandState s] a -> (a,s)
runStateBack0 m =
  let (x,s) = go m s in
  (x,s)
 where
   go :: Eff '[OnDemandState s] a -> s -> (a,s)
   go (Val x) s = (x,s)
   go (E q u) s0 = case decomp u of
     Right Get      -> k s0 s0
     Right (Put s1)  -> let ~(x,sp) = k () sp in (x,s1)
     Right (Delay m1) -> let ~(x,s1) = go m1 s0 in k x s1
     Left _ -> error "Impossible happened: Nothing to relay!"
     where
       k = qComp q go

-- | Another implementation, exploring Haskell's laziness to make putAttr
-- also technically inherited, to accumulate the sequence of
-- updates. This implementation is compatible with deep handlers, and
-- lets us play with different notions of `backwardness'
runStateBack :: Eff '[OnDemandState s] a -> (a,s)
runStateBack m =
  let (x,(_,sp)) = run $ go m (sp,[]) in
  (x,head sp)
 where
   go :: Eff '[OnDemandState s] a -> ([s],[s]) -> Eff '[] (a,([s],[s]))
   go = handle_relay' S.withState h
   h k Get s0@(sg, _) = k (head sg) s0
   h k (Put s1) (sg, sp) = k () (tail sg,sp++[s1])
   h k (Delay m1) s0 = let ~(x,s1) = run $ go m1 s0 in k x s1

-- ^ A different notion of `backwards' is realized if we change the Put
-- handler slightly. How?
