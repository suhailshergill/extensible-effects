{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.State.Lazy.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.State.Lazy
import Data.OpenUnion
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Lazy1_State_runState :: Assertion
case_Lazy1_State_runState = let
  (r, ()) = run
            $ flip runState undefined
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
