{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- On-demand state computation:
-- example taken from Edward Kmett's comment here:
-- <http://www.reddit.com/r/haskell/comments/387ex0/are_extensible_effects_a_complete_replacement_for/crt1pzm>

-- Extensible effects make it clear that where the computation is delayed
-- (which I take as an advantage) and they do maintain the degree of
-- extensibility (the delayed computation must be effect-closed, but the
-- whole computation does not have to be).

module LazyState where

import Eff1
import OpenUnion51


-- Define a new effect for state on-demand (in ExtEff, the state is
-- by default strict -- as it should be if we want the predictable performance
-- and effect sequencing)

data LazyState s v where
  LGet  :: LazyState s s
  LPut  :: s -> LazyState s ()
  Delay :: Eff '[LazyState s] a  -> LazyState s a --  Eff as a transformer

-- Primitive state operations
lget = send LGet
lput = send . LPut

lmodify f = do
  s <- lget
  lput (f s)

onDemand :: Member (LazyState s) r => Eff '[LazyState s] v -> Eff r v
onDemand = send . Delay

-- The handler
runStateLazy :: s -> Eff (LazyState s ': r) a -> Eff r (a,s)
runStateLazy s = handle_relay_s s (\s x -> return (x,s))
                   (\s req k -> case req of
                       LGet    -> k s s
                       LPut s  -> k s ()
                       Delay m -> let ~(x,s1) = run $ runStateLazy s m
                                  in k s1 x)
                   

-- Tests

lex1 = do
  onDemand lex1
  lput (1::Int)

ex1Run :: ((),Int)
ex1Run = run $ runStateLazy 0 lex1 
-- ((),1)

-- Edward's example

lex3 = do
  onDemand lex3
  lmodify ((1::Int):)

ex3Run = let (x,s) = run $ runStateLazy (undefined::[Int]) lex3
         in (x,take 5 s)

-- ((),[1,1,1,1,1])

-- a bit more interesting
lex4 :: Eff '[LazyState [Int]] [Int]
lex4 = do
  lmodify ((0::Int):)
  onDemand lex4
  lmodify ((1::Int):)
  onDemand (onDemand lex4 :: Eff '[LazyState [Int]] [Int])
  lmodify ((2::Int):)
  lmodify ((3::Int):)
  lget

ex4Run :: ([Int],[Int])
ex4Run = let (x,s) = run $ runStateLazy [] lex4
         in (take 7 $ x,take 5 $ s)

-- ([3,2,3,2,3,2,3],[3,2,3,2,3])

-- Edward's example plus exceptions
lex31 :: Member (LazyState [Int]) r => Eff r ()
lex31 = do
  onDemand (lex31 :: Eff '[LazyState [Int]] ())
  lmodify ((1::Int):)

lex5 = do
  lex31
  x <- lget
  throwError ((take 5 x)::[Int])

ex5Run :: Either [Int] a
ex5Run = fst . run . runStateLazy (undefined::[Int]) . runError $ lex5
-- Left [1,1,1,1,1]

ex51Run :: Either [Int] (a,[Int])
ex51Run = run . runError . runStateLazy (undefined::[Int]) $ lex5
-- Left [1,1,1,1,1]

-- Backwards state
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
         Right (LPut s)  -> let ~(x,sp) = go sp $ qApp q () in (x,s)
         Right (Delay m) -> let ~(x,s1) = go s m in go s1 $ qApp q x

-- Another implementation, exploring Haskell's laziness to make putAttr
-- also technically inherited, to accumulate the sequence of
-- updates. This implementation is compatible with deep handlers, and
-- lets us play with different notions of `backwardness'
runStateBack :: Eff '[LazyState s] a -> (a,s)
runStateBack m =
  let (x,(sg,sp)) = run $ go (sp,[]) m in
  (x,head sp)
 where
   go :: ([s],[s]) -> Eff '[LazyState s] a -> Eff '[] (a,([s],[s]))
   go ss = handle_relay_s ss (\ss x -> return (x,ss))
                   (\ss@(sg,sp) req k -> case req of
                       LGet    -> k ss (head sg)
                       LPut s  -> k (tail sg,sp++[s]) ()
                       Delay m -> let ~(x,ss1) = run $ go ss m
                                  in k ss1 x)

-- A different notion of `backwards' is realized if we change the LPut
-- handler slightly. How?

st = do
  x <- lget
  lput (1::Int)
  lput (1::Int)
  y <- lget
  lput (2::Int)
  lput (10::Int)
  lput (3::Int)
  z <- lget
  lput (4::Int)
  return (x,y,z)

stF :: ((Int,Int,Int),Int)
stF = run $ runStateLazy (0::Int) st
-- ((0,1,3),4)

stB0 :: ((Int,Int,Int),Int)
stB0 = runStateBack0 st
-- ((1,2,4),1)

stB :: ((Int,Int,Int),Int)
stB = runStateBack st
-- ((1,2,4),1)

ones :: [Int]
ones = snd $ runStateBack $ do
  s <- lget
  lput ((1::Int):s)

-- take 5 ones
-- [1,1,1,1,1]

