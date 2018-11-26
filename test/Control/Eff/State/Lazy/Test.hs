{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.State.Lazy.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.State.Lazy
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Lazy1_State_runState :: Assertion
case_Lazy1_State_runState = let
  (r, ()) = run
            $ runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertNoUndefined r
  where
    getVoid :: Eff '[State ()] ()
    getVoid = get

    putVoid :: () -> Eff '[State ()] ()
    putVoid = put

case_Lazy1_State_monadBaseControl :: Assertion
case_Lazy1_State_monadBaseControl = runLift (runState i (doThing $ modify f)) @=? Just ((), i + 1)
  where
    i = 0 :: Int
    f = succ :: Int -> Int
