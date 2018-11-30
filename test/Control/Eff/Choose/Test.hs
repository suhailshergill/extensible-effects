{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Choose.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Example
import Control.Eff.Example.Test (ex2)
import Control.Eff.Exception
import Control.Eff.Choose
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Choose1_exc11 :: Assertion
case_Choose1_exc11 = [2,3] @=? (run exc11)
  where
    exc11 = makeChoice exc1
    exc1 = return 1 `add` choose [1,2]

case_Choose_ex2 :: Assertion
case_Choose_ex2 =
  let ex2_1 = run . makeChoice . runErrBig $ ex2 (choose [5,7,1])
      ex2_2 = run . runErrBig . makeChoice $ ex2 (choose [5,7,1])
  in
    assertEqual "Choose: Combining exceptions and non-determinism: ex2_1"
    expected1 ex2_1
    >> assertEqual "Choose: Combining exceptions and non-determinism: ex2_2"
    expected2 ex2_2
  where
    expected1 = [Right 5,Left (TooBig 7),Right 1]
    expected2 = Left (TooBig 7)

case_Choose_exRec :: Assertion
case_Choose_exRec =
  let exRec_1 = run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,1]))
      exRec_2 = run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,1]))
      exRec_3 = run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,11,1]))
      exRec_4 = run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,11,1]))
  in
    assertEqual "Choose: error recovery: exRec_1" expected1 exRec_1
    >> assertEqual "Choose: error recovery: exRec_2" expected2 exRec_2
    >> assertEqual "Choose: error recovery: exRec_3" expected3 exRec_3
    >> assertEqual "Choose: error recovery: exRec_4" expected4 exRec_4
  where
    expected1 = Right [5,7,1]
    expected2 = [Right 5,Right 7,Right 1]
    expected3 = Left (TooBig 11)
    expected4 = [Right 5,Right 7,Left (TooBig 11),Right 1]
    -- Errror recovery part
    -- The code is the same as in transf1.hs. The inferred signatures differ
    -- Was: exRec :: MonadError TooBig m => m Int -> m Int
    -- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
    exRec m = catchError m handler
      where handler (TooBig n) | n <= 7 = return n
            handler e = throwError e

case_Choose_monadBaseControl :: Assertion
case_Choose_monadBaseControl = runLift (makeChoice $ doThing $ choose [1,2,3]) @=? Just [1,2,3]
