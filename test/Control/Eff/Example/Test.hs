{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Eff.Example.Test (testGroups, ex2) where

import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Control.Eff
import Control.Eff.Example
import Control.Eff.Exception
import Control.Eff.Reader.Lazy
import Control.Eff.Writer.Lazy
import Control.Eff.State.Lazy
import Utils

testGroups = [ $(testGroupGenerator) ]

-- The type is inferred
-- ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

case_Exception1_ex2r :: Assertion
case_Exception1_ex2r = (Right 5) @=? (run ex2r)
  where
    ex2r = runReader (5::Int) (runErrBig (ex2 ask))

case_Exception1_ex2r1 :: Assertion
case_Exception1_ex2r1 = (Left (TooBig 7)) @=? (run ex2r1)
  where
    ex2r1 = runReader (7::Int) (runErrBig (ex2 ask))

-- Different order of handlers (layers)
case_Exception1_ex2r2 :: Assertion
case_Exception1_ex2r2 = (Left (TooBig 7)) @=? (run ex2r2)
  where
    ex2r2 = runErrBig (runReader (7::Int) (ex2 ask))

case_multiple_eff_sum2 :: Assertion
case_multiple_eff_sum2 =
  assertEqual "Int : Float" 33 intThenFloat
  >> assertEqual "Float : Int" intThenFloat floatThenInt
  where
    intThenFloat = run $ runReader (20::Float) (runReader (10::Int) sum2)
    floatThenInt = run $ runReader (10::Int) (runReader (20::Float) sum2)

prop_Documentation_example :: [Integer] -> Property
prop_Documentation_example l = let
  ((), total1) = run $ runState 0 (sumAll l)
  ((), last1) = run $ runLastWriter $ writeAll l
  (((), last2), total2) = run $ runState 0 (runLastWriter (writeAndAdd l))
  (((), total3), last3) = run $ runLastWriter $ runState 0 (writeAndAdd l)
  in
   allEqual [safeLast l, last1, last2, last3]
   .&&. allEqual [sum l, total1, total2, total3]
