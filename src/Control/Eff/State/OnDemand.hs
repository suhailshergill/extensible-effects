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
import Control.Eff.Lift

import Control.Eff.Writer.Lazy
import Control.Eff.Reader.Lazy

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

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
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
runState' s =
  handle_relay_s s
  (\s0 x -> return (x,s0))
  (\s0 sreq k -> case sreq of
      Get    -> k s0 s0
      Put s1 -> k s1 ()
      Delay m1 -> let ~(x,s1) = run $ runState' s0 m1
                  in k s1 x)

-- Since State is so frequently used, we optimize it a bit
-- | Run a State effect
runState :: s                            -- ^ Initial state
         -> Eff (OnDemandState s ': r) w -- ^ Effect incorporating State
         -> Eff r (w,s)                  -- ^ Effect containing final state and a return value
runState s (Val x) = return (x,s)
runState s0 (E u0 q) = case decomp u0 of
  Right Get     -> runState s0 (q ^$ s0)
  Right (Put s1) -> runState s1 (q ^$ ())
  Right (Delay m1) -> let ~(x,s1) = run $ runState s0 m1
                      in runState s1 (q ^$ x)
  Left  u -> E u (singleK (\x -> runState s0 (q ^$ x)))

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
runStateR s0 m0 = loop s0 m0
 where
   loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w,s)
   loop s (Val x) = return (x,s)
   loop s (E u0 q) = case decomp u0 of
     Right (Tell w) -> k w ()
     Left  u  -> case decomp u of
       Right Ask -> k s s
       Left u1 -> E u1 (singleK (k s))
    where k x = qComp q (loop x)

-- | Backwards state
-- The overall state is represented with two attributes: the inherited
-- getAttr and the synthesized putAttr.
-- At the root node, putAttr becomes getAttr, tying the knot.
-- As usual, the inherited attribute is the argument (i.e., the `environment')
-- and the synthesized is the result of the handler |go| below.
runStateBack0 :: Eff '[OnDemandState s] a -> (a,s)
runStateBack0 m =
  let (x,s) = go s m in
  (x,s)
 where
   go :: s -> Eff '[OnDemandState s] a -> (a,s)
   go s (Val x) = (x,s)
   go s0 (E u q) = case decomp u of
         Right Get      -> go s0 $ (q ^$ s0)
         Right (Put s1)  -> let ~(x,sp) = go sp $ (q ^$ ()) in (x,s1)
         Right (Delay m1) -> let ~(x,s1) = go s0 m1 in go s1 $ (q ^$ x)
         Left _ -> error "Impossible happened: Union []"

-- | Another implementation, exploring Haskell's laziness to make putAttr
-- also technically inherited, to accumulate the sequence of
-- updates. This implementation is compatible with deep handlers, and
-- lets us play with different notions of `backwardness'
runStateBack :: Eff '[OnDemandState s] a -> (a,s)
runStateBack m =
  let (x,(_sg,sp)) = run $ go (sp,[]) m in
  (x,head sp)
 where
   go :: ([s],[s]) -> Eff '[OnDemandState s] a -> Eff '[] (a,([s],[s]))
   go ss = handle_relay_s ss (\ss0 x -> return (x,ss0))
                   (\ss0@(sg,sp) req k -> case req of
                       Get    -> k ss0 (head sg)
                       Put s1  -> k (tail sg,sp++[s1]) ()
                       Delay m1 -> let ~(x,ss1) = run $ go ss0 m1
                                   in k ss1 x)

-- ^ A different notion of `backwards' is realized if we change the Put
-- handler slightly. How?
