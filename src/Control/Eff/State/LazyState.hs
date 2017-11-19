{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | On-demand state computation:
-- example taken from Edward Kmett's comment here:
-- <http://www.reddit.com/r/haskell/comments/387ex0/are_extensible_effects_a_complete_replacement_for/crt1pzm>
--
-- Extensible effects make it clear that where the computation is delayed and
-- they do maintain the degree of extensibility (the delayed computation must be
-- effect-closed, but the whole computation does not have to be).
module Control.Eff.State.LazyState where

import Control.Eff
import Data.OpenUnion

-- | Define a new effect for state on-demand (in ExtEff, the state is
-- by default strict -- as it should be if we want the predictable performance
-- and effect sequencing)
data LazyState s v where
  LGet  :: LazyState s s
  LPut  :: s -> LazyState s ()
  Delay :: Eff '[LazyState s] a  -> LazyState s a --  Eff as a transformer

-- | Primitive state operations
lget :: Member (LazyState s) r => Eff r s
lget = send LGet

lput :: Member (LazyState s) r => s -> Eff r ()
lput = send . LPut

lmodify :: (Member (LazyState s) r, Member (LazyState t) r)
        => (t -> s) -> Eff r ()
lmodify f = do
  s <- lget
  lput (f s)

onDemand :: Member (LazyState s) r => Eff '[LazyState s] v -> Eff r v
onDemand = send . Delay

-- | The handler
runStateLazy :: s -> Eff (LazyState s ': r) a -> Eff r (a,s)
runStateLazy s = handle_relay_s s (\s0 x -> return (x,s0))
                   (\s0 req k -> case req of
                       LGet    -> k s0 s0
                       LPut s1  -> k s1 ()
                       Delay m -> let ~(x,s1) = run $ runStateLazy s0 m
                                  in k s1 x)

-- | Backwards state
-- The overall state is represented with two attributes: the inherited
-- getAttr and the synthesized putAttr.
-- At the root node, putAttr becomes getAttr, tying the knot.
-- As usual, the inherited attribute is the argument (i.e., the `environment')
-- and the synthesized is the result of the handler |go| below.
runStateBack0 :: Eff '[LazyState s] a -> (a,s)
runStateBack0 m =
  let (x,s) = go s m in
  (x,s)
 where
   go :: s -> Eff '[LazyState s] a -> (a,s)
   go s (Val x) = (x,s)
   go s (E u q) = case decomp u of
         Right LGet      -> go s $ qApp q s
         Right (LPut s1)  -> let ~(x,sp) = go sp $ qApp q () in (x,s1)
         Right (Delay m1) -> let ~(x,s1) = go s m1 in go s1 $ qApp q x
         Left _ -> error "LazyState: the impossible happened"

-- | Another implementation, exploring Haskell's laziness to make putAttr
-- also technically inherited, to accumulate the sequence of
-- updates. This implementation is compatible with deep handlers, and
-- lets us play with different notions of `backwardness'
runStateBack :: Eff '[LazyState s] a -> (a,s)
runStateBack m =
  let (x,(_,sp)) = run $ go (sp,[]) m in
  (x,head sp)
 where
   go :: ([s],[s]) -> Eff '[LazyState s] a -> Eff '[] (a,([s],[s]))
   go ss = handle_relay_s ss (\ss1 x -> return (x,ss1))
                   (\ss1@(sg,sp) req k -> case req of
                       LGet    -> k ss1 (head sg)
                       LPut s  -> k (tail sg,sp++[s]) ()
                       Delay m1 -> let ~(x,ss2) = run $ go ss1 m1
                                  in k ss2 x)

-- A different notion of `backwards' is realized if we change the LPut
-- handler slightly. How?
