{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing #-}

module Control.Eff.State.LazyState.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.State.LazyState
import Data.OpenUnion

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_LazierState_ex1 :: Assertion
case_LazierState_ex1 =
  let actual = run $ runStateLazy 0 lex1
  in
    assertEqual "LazyState: ex1"
    ((), 1::Int) actual
  where
    lex1 = do
      onDemand lex1
      lput (1::Int)

case_LazierState_ex3 :: Assertion
case_LazierState_ex3 =
  let (x,s) = run $ runStateLazy (undefined::[Int]) lex3
  in assertEqual "LazyState: ex3"
     ((),[1,1,1,1,1]) (x,take 5 s)
  where
    lex3 = do
      onDemand lex3
      lmodify ((1::Int):)

-- a bit more interesting
case_LazierState_ex4 =
  let (x,s) = run $ runStateLazy [] lex4
  in assertEqual "LazyState: ex4"
     expect (take 7 $ x,take 5 $ s)
  where
    expect = ([3,2,3,2,3,2,3],[3,2,3,2,3])
    lex4 :: Eff '[LazyState [Int]] [Int]
    lex4 = do
      lmodify ((0::Int):)
      onDemand lex4
      lmodify ((1::Int):)
      onDemand (onDemand lex4 :: Eff '[LazyState [Int]] [Int])
      lmodify ((2::Int):)
      lmodify ((3::Int):)
      lget


-- Edward's example plus exceptions
case_LazierState_ex5 :: Assertion
case_LazierState_ex5 =
  let
    -- the annotations below are needed for assertEqual
    ex5Run :: Either [Int] () = fst . run . runStateLazy (undefined::[Int]) . runExc $ lex5
    ex51Run :: Either [Int] ((), [Int]) = run . runExc . runStateLazy (undefined::[Int]) $ lex5
  in
    assertEqual "LazyState ex5" (Left ones) ex5Run
    >> assertEqual "LazyState ex51" (Left ones) ex51Run
  where
    ones = take 5 $ repeat (1::Int)
    lex31 :: Member (LazyState [Int]) r => Eff r ()
    lex31 = do
      onDemand (lex31 :: Eff '[LazyState [Int]] ())
      lmodify ((1::Int):)

    lex5 = do
      lex31
      x <- lget
      throwExc ((take 5 x)::[Int])

case_LazierState_st :: Assertion
case_LazierState_st = let
  stF :: ((Int,Int,Int),Int) = run $ runStateLazy (0::Int) st
  stB0 :: ((Int,Int,Int),Int) = runStateBack0 st
  stB :: ((Int,Int,Int),Int) = runStateBack st
  in
    assertEqual "LazyState stF" ((0,1,3),4) stF
    >> assertEqual "LazyState stB0" ((1,2,4),1) stB0
    >> assertEqual "LazyState stB" ((1,2,4),1) stB
  where
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

case_LazierState_ones :: Assertion
case_LazierState_ones =
  let ones :: [Int] = snd $ runStateBack $ do
        s <- lget
        lput ((1::Int):s)
  in
    assertEqual "LazyState ones" [1,1,1,1,1] (take 5 ones)
