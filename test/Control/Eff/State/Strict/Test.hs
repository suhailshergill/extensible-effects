{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.State.Strict.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Control.Eff.Writer.Strict
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Strict1_State_runState :: Assertion
case_Strict1_State_runState = let
  (r, ()) = run
            $ runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertUndefined r
  where
    getVoid :: Eff '[State ()] ()
    getVoid = get

    putVoid :: () -> Eff '[State ()] ()
    putVoid = put

case_Strict1_State_ts1 :: Assertion
case_Strict1_State_ts1 = (10,10) @=? (run (runState (0::Int) ts1))
  where
    ts1 = do
      put (10 ::Int)
      x <- get
      return (x::Int)

case_Strict1_State_ts11 :: Assertion
case_Strict1_State_ts11 =
  (10,10) @=? (run (runStateR (0::Int) ts11))
  where
    ts11 = do
      tell (10 ::Int)
      x <- ask
      return (x::Int)

case_Strict1_State_ts2 :: Assertion
case_Strict1_State_ts2 = (30::Int,20::Int) @=?
  (run (runState (0::Int) ts2))
  where
    ts2 = do
      put (10::Int)
      x <- get
      put (20::Int)
      y <- get
      return (x+y)

case_Strict1_State_ts21 :: Assertion
case_Strict1_State_ts21 = (30::Int,20::Int) @=?
  (run (runStateR (0::Int) ts21))
  where
    ts21 = do
      tell (10::Int)
      x <- ask
      tell (20::Int)
      y <- ask
      return (x+y)

tes1 :: (Member (State Int) r
        , Member (Exc [Char]) r) => Eff r b
tes1 = do
  incr
  throwError "exc"
  where
    incr = get >>= put . (+ (1::Int))

case_Strict1_State_ter1 :: Assertion
case_Strict1_State_ter1 = (Left "exc" :: Either String Int,2) @=?
  (run $ runState (1::Int) (runError tes1))

case_Strict1_State_ter2 :: Assertion
case_Strict1_State_ter2 = (Left "exc" :: Either String (Int,Int)) @=?
  (run $ runError (runState (1::Int) tes1))

teCatch :: Member (Exc String) r => Eff r a -> Eff r [Char]
teCatch m = catchError (m >> return "done") (\e -> return (e::String))

case_Strict1_State_ter3 :: Assertion
case_Strict1_State_ter3 = (Right "exc" :: Either String String,2) @=?
  (run $ runState (1::Int) (runError (teCatch tes1)))

case_Strict1_State_ter4 :: Assertion
case_Strict1_State_ter4 = (Right ("exc",2) :: Either String (String,Int)) @=?
  (run $ runError (runState (1::Int) (teCatch tes1)))

case_Strict1_State_monadBaseControl :: Assertion
case_Strict1_State_monadBaseControl = runLift (runState i (doThing $ modify f)) @=?
  Just ((), i + 1)
  where
    i = 0 :: Int
    f = succ :: Int -> Int
