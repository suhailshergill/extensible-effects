{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- | Lazy state effect
module Control.Eff.State.Lazy1 where

import Control.Eff1 hiding (State(..), get, put, runState', runState
                           , Reader(..), Writer(..))
import Control.Eff.Writer.Lazy1
import Control.Eff.Reader.Lazy1
import Data.OpenUnion51
import Data.FTCQueue1

-- ------------------------------------------------------------------------
-- | State, lazy (i.e., on-demand)
--
-- Extensible effects make it clear that where the computation is delayed
-- (which I take as an advantage) and they do maintain the degree of
-- extensibility (the delayed computation must be effect-closed, but the
-- whole computation does not have to be).
data State s v where
  Get  :: State s s
  Put  :: s -> State s ()
  Delay :: Eff '[State s] a  -> State s a --  Eff as a transformer

-- | Return the current value of the state. The signatures are inferred
get :: Member (State s) r => Eff r s
get = send Get
{-# RULES
  "get/bind" forall k. get >>= k = send Get >>= k
 #-}

-- | Write a new value of the state.
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

onDemand :: Member (State s) r => Eff '[State s] v -> Eff r v
onDemand = send . Delay

runState' :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState' m s =
  handle_relay_s s
  (\s0 x -> return (x,s0))
  (\s0 sreq k -> case sreq of
      Get    -> k s0 s0
      Put s1 -> k s1 ()
      Delay m1 -> let ~(x,s1) = run $ runState' m1 s0
                  in k s1 x)
  m

-- Since State is so frequently used, we optimize it a bit
runState :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState (Val x) s = return (x,s)
runState (E u0 q) s0 = case decomp u0 of
  Right Get     -> runState (qApp q s0) s0
  Right (Put s1) -> runState (qApp q ()) s1
  Right (Delay m1) -> let ~(x,s1) = run $ runState m1 s0
                      in runState (qApp q x) s1
  Left  u -> E u (tsingleton (\x -> runState (qApp q x) s0))

-- | Transform the state with a function.
modify :: (Member (State s) r) => (s -> s) -> Eff r ()
modify f = get >>= put . f

-- | Run a State effect, discarding the final state.
evalState :: Eff (State s ': r) w -> s -> Eff r w
evalState m s = fmap fst . flip runState s $ m

-- | Run a State effect and return the final state.
execState :: Eff (State s ': r) w -> s -> Eff r s
execState m s = fmap snd . flip runState s $ m

-- | A different representation of State: decomposing State into mutation
-- (Writer) and Reading. We don't define any new effects: we just handle the
-- existing ones.  Thus we define a handler for two effects together.
runStateR :: Eff (Writer s ': Reader s ': r) w -> s -> Eff r (w,s)
runStateR m0 s0 = loop s0 m0
 where
   loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w,s)
   loop s (Val x) = return (x,s)
   loop s (E u0 q) = case decomp u0 of
     Right (Writer w v) -> k w v
     Left  u  -> case decomp u of
       Right (Reader f) -> k s (f s)
       Left u1 -> E u1 (tsingleton (k s))
    where k x = qComp q (loop x)

-- | Backwards state
-- The overall state is represented with two attributes: the inherited
-- getAttr and the synthesized putAttr.
-- At the root node, putAttr becomes getAttr, tying the knot.
-- As usual, the inherited attribute is the argument (i.e., the `environment')
-- and the synthesized is the result of the handler |go| below.
runStateBack0 :: Eff '[State s] a -> (a,s)
runStateBack0 m =
  let (x,s) = go s m in
  (x,s)
 where
   go :: s -> Eff '[State s] a -> (a,s)
   go s (Val x) = (x,s)
   go s0 (E u q) = case decomp u of
         Right Get      -> go s0 $ qApp q s0
         Right (Put s1)  -> let ~(x,sp) = go sp $ qApp q () in (x,s1)
         Right (Delay m1) -> let ~(x,s1) = go s0 m1 in go s1 $ qApp q x
         Left _ -> error "Impossible happened: Union []"

-- | Another implementation, exploring Haskell's laziness to make putAttr
-- also technically inherited, to accumulate the sequence of
-- updates. This implementation is compatible with deep handlers, and
-- lets us play with different notions of `backwardness'
runStateBack :: Eff '[State s] a -> (a,s)
runStateBack m =
  let (x,(_sg,sp)) = run $ go (sp,[]) m in
  (x,head sp)
 where
   go :: ([s],[s]) -> Eff '[State s] a -> Eff '[] (a,([s],[s]))
   go ss = handle_relay_s ss (\ss0 x -> return (x,ss0))
                   (\ss0@(sg,sp) req k -> case req of
                       Get    -> k ss0 (head sg)
                       Put s1  -> k (tail sg,sp++[s1]) ()
                       Delay m1 -> let ~(x,ss1) = run $ go ss0 m1
                                   in k ss1 x)

-- ^ A different notion of `backwards' is realized if we change the Put
-- handler slightly. How?
