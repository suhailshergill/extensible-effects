{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Control.Eff.State.OnDemand.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.State.OnDemand
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_LazierState_ex1 :: Assertion
case_LazierState_ex1 =
  let actual = run $ runState 0 lex1
  in
    assertEqual "OnDemandState: ex1"
    ((), 1::Int) actual
  where
    lex1 = do
      onDemand lex1
      put (1::Int)

case_LazierState_ex3 :: Assertion
case_LazierState_ex3 =
  let (x,s) = run $ runState (undefined::[Int]) lex3
  in assertEqual "OnDemandState: ex3"
     ((),[1,1,1,1,1]) (x,take 5 s)
  where
    lex3 = do
      onDemand lex3
      modify ((1::Int):)

-- a bit more interesting
case_LazierState_ex4 =
  let (x,s) = run $ runState [] lex4
  in assertEqual "OnDemandState: ex4"
     expect (take 7 $ x,take 5 $ s)
  where
    expect = ([3,2,3,2,3,2,3],[3,2,3,2,3])
    lex4 :: Eff '[OnDemandState [Int]] [Int]
    lex4 = do
      modify ((0::Int):)
      onDemand lex4
      modify ((1::Int):)
      onDemand (onDemand lex4 :: Eff '[OnDemandState [Int]] [Int])
      modify ((2::Int):)
      modify ((3::Int):)
      get


-- Edward's example plus exceptions
case_LazierState_ex5 :: Assertion
case_LazierState_ex5 =
  let
    -- the annotations below are needed for assertEqual
    ex5Run :: Either [Int] () = fst . run $ runState (undefined::[Int]) (runError lex5)
    ex51Run :: Either [Int] ((), [Int]) = run $ runError $ runState (undefined::[Int]) lex5
  in
    assertEqual "OnDemandState ex5" (Left ones) ex5Run
    >> assertEqual "OnDemandState ex51" (Left ones) ex51Run
  where
    ones = take 5 $ repeat (1::Int)
    lex31 :: Member (OnDemandState [Int]) r => Eff r ()
    lex31 = do
      onDemand (lex31 :: Eff '[OnDemandState [Int]] ())
      modify ((1::Int):)

    lex5 = do
      lex31
      x <- get
      throwError ((take 5 x)::[Int])

case_LazierState_st :: Assertion
case_LazierState_st = let
  stF :: ((Int,Int,Int),Int) = run $ runState (0::Int) st
  stB0 :: ((Int,Int,Int),Int) = runStateBack0 st
  stB :: ((Int,Int,Int),Int) = runStateBack st
  in
    assertEqual "OnDemandState stF" ((0,1,3),4) stF
    >> assertEqual "OnDemandState stB0" ((1,2,4),1) stB0
    >> assertEqual "OnDemandState stB" ((1,2,4),1) stB
  where
    st = do
      x <- get
      put (1::Int)
      put (1::Int)
      y <- get
      put (2::Int)
      put (10::Int)
      put (3::Int)
      z <- get
      put (4::Int)
      return (x,y,z)

case_LazierState_ones :: Assertion
case_LazierState_ones =
  let ones :: [Int] = snd $ runStateBack $ do
        s <- get
        put ((1::Int):s)
  in
    assertEqual "OnDemandState ones" [1,1,1,1,1] (take 5 ones)

case_LazierState_monadBaseControl :: Assertion
case_LazierState_monadBaseControl = runLift (runState i (doThing $ modify f)) @=? Just ((), i + 1)
  where
    i = 0 :: Int
    f = succ :: Int -> Int
