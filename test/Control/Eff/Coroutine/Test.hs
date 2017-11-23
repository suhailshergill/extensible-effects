{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Control.Eff.Coroutine.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Coroutine
import Control.Eff.Trace
import Control.Eff.Reader.Strict
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

yieldInt :: Member (Yield Int ()) r => Int -> Eff r ()
yieldInt = yield

case_Coroutines_c1 :: Assertion
case_Coroutines_c1 = do
  ((), actual) <- catchOutput c1
  assertEqual
    "Coroutine: Simple coroutines using Eff"
    (unlines ["1", "2", "Done"]) actual
  where
    th1 :: Member (Yield Int ()) r => Eff r ()
    th1 = yieldInt 1 >> yieldInt 2

    c1 = runTrace (loop =<< runC th1)
      where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
            loop (Done)    = trace ("Done")

case_Coroutines_c2 :: Assertion
case_Coroutines_c2 = do
  ((), actual1) <- catchOutput c2
  assertEqual "Coroutine: Add dynamic variables"
    (unlines ["10", "10", "Done"]) actual1
  ((), actual2) <- catchOutput c21
  assertEqual "Coroutine: locally changing the dynamic environment for the suspension"
    (unlines ["10", "11", "Done"]) actual2
  where
    -- The code is essentially the same as that in transf.hs (only added
    -- a type specializtion on yield). The inferred signature is different though.
    -- Before it was
    --    th2 :: MonadReader Int m => CoT Int m ()
    -- Now it is more general:
    th2 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
    th2 = ask >>= yieldInt >> (ask >>= yieldInt)

    -- Code is essentially the same as in transf.hs; no liftIO though
    c2 = runTrace $ runReader (loop =<< runC th2) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
            loop Done    = trace "Done"

    -- locally changing the dynamic environment for the suspension
    c21 = runTrace $ runReader (loop =<< runC th2) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
            loop Done    = trace "Done"

case_Coroutines_c3 :: Assertion
case_Coroutines_c3 = do
  ((), actual1) <- catchOutput c3
  assertEqual "Coroutine: two sorts of local rebinding"
    (unlines ["10", "10", "20", "20", "Done"]) actual1
  ((), actual2) <- catchOutput c31
  let expected2 = (unlines ["10", "11", "21", "21", "Done"])
  assertEqual "Coroutine: locally changing the dynamic environment for the suspension"
    expected2 actual2
  ((), actual3) <- catchOutput c4
  assertEqual "Coroutine: abstracting the client computation"
    expected2 actual3
  where
    th3 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
    th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
      where ay = ask >>= yieldInt

    c3 = runTrace $ runReader (loop =<< runC th3) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
            loop Done    = trace "Done"

    -- The desired result: the coroutine shares the dynamic environment with its
    -- parent; however, when the environment is locally rebound, it becomes
    -- private to coroutine.
    c31 = runTrace $ runReader (loop =<< runC th3) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
            loop Done    = trace "Done"

    -- We now make explicit that the client computation, run by th4,
    -- is abstract. We abstract it out of th4
    c4 = runTrace $ runReader (loop =<< runC (th4 client)) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
            loop Done    = trace "Done"

            -- cl, client, ay are monomorphic bindings
            th4 cl = cl >> local (+(10::Int)) cl
            client = ay >> ay
            ay     = ask >>= yieldInt

case_Corountines_c5 :: Assertion
case_Corountines_c5 = do
  ((), actual) <- catchOutput c5
  let expected = unlines ["10"
                         ,"11"
                         ,"12"
                         ,"18"
                         ,"18"
                         ,"18"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"Done"
                         ]
  assertEqual "Corountine: Even more dynamic example"
    expected actual
  where
    c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (\_y->x+1) (k ()) >>= loop
            loop Done    = trace "Done"

            -- cl, client, ay are monomorphic bindings
            client = ay >> ay >> ay
            ay     = ask >>= yieldInt

            -- There is no polymorphic recursion here
            th cl = do
              cl
              v <- ask
              (if v > (20::Int) then id else local (+(5::Int))) cl
              if v > (20::Int) then return () else local (+(10::Int)) (th cl)

case_Coroutines_c7 :: Assertion
case_Coroutines_c7 = do
  ((), actual) <- catchOutput c7
  let expected = unlines ["1010"
                         ,"1021"
                         ,"1032"
                         ,"1048"
                         ,"1064"
                         ,"1080"
                         ,"1101"
                         ,"1122"
                         ,"1143"
                         ,"1169"
                         ,"1195"
                         ,"1221"
                         ,"1252"
                         ,"1283"
                         ,"1314"
                         ,"1345"
                         ,"1376"
                         ,"1407"
                         ,"Done"
                         ]
  assertEqual "Coroutine: And even more dynamic example"
    expected actual
  where
    c7 = runTrace $
          runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
     where loop (Y x k) = trace (show (x::Int)) >>
                          local (\_y->fromIntegral (x+1)::Double) (k ()) >>= loop
           loop Done    = trace "Done"

           -- cl, client, ay are monomorphic bindings
           client = ay >> ay >> ay
           ay     = ask >>= \x -> ask >>=
                     \y -> yieldInt (x + round (y::Double))

           -- There is no polymorphic recursion here
           th cl = do
             cl
             v <- ask
             (if v > (20::Int) then id else local (+(5::Int))) cl
             if v > (20::Int) then return () else local (+(10::Int)) (th cl)

case_Coroutines_c7' :: Assertion
case_Coroutines_c7' = do
  ((), actual) <- catchOutput c7'
  let expected = unlines ["1010"
                         ,"1021"
                         ,"1032"
                         ,"1048"
                         ,"1048"
                         ,"1048"
                         ,"1069"
                         ,"1090"
                         ,"1111"
                         ,"1137"
                         ,"1137"
                         ,"1137"
                         ,"1168"
                         ,"1199"
                         ,"1230"
                         ,"1261"
                         ,"1292"
                         ,"1323"
                         ,"Done"
                         ]
  assertEqual "Coroutine: And even more dynamic example"
    expected actual
  where
    c7' = runTrace $
          runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
     where loop (Y x k) = trace (show (x::Int)) >>
                          local (\_y->fromIntegral (x+1)::Double) (k ()) >>= loop
           loop Done    = trace "Done"

           -- cl, client, ay are monomorphic bindings
           client = ay >> ay >> ay
           ay     = ask >>= \x -> ask >>=
                     \y -> yieldInt (x + round (y::Double))

           -- There is no polymorphic recursion here
           th cl = do
             cl
             v <- ask
             (if v > (20::Int) then id else local (+(5::Double))) cl
             if v > (20::Int) then return () else local (+(10::Int)) (th cl)
