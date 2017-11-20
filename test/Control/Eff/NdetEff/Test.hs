{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.NdetEff.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.NdetEff
import Control.Eff.Writer.Strict
import Control.Monad (liftM2, msum, guard, mzero, mplus)
import Data.OpenUnion
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_NdetEff_testCA :: Assertion
case_NdetEff_testCA = [2, 4..10] @=? (run $ makeChoiceA testCA)
  where
    testCA :: (Integral a) => Eff (NdetEff ': r) a
    testCA = do
      i <- msum . fmap return $ [1..10]
      guard (i `mod` 2 == 0)
      return i

case_NdetEff_ifte :: Assertion
case_NdetEff_ifte =
  let primes = ifte_test_run
  in
    assertEqual "NdetEff: test ifte using primes"
    [2,3,5,7,11,13,17,19,23,29] primes
  where
    ifte_test = do
      n <- gen
      ifte (do
               d <- gen
               guard $ d < n && n `mod` d == 0
               -- _ <- trace ("d: " ++ show d) (return ())
           )
        (\_ -> mzero)
        (return n)
        where gen = msum . fmap return $ [2..30]

    ifte_test_run :: [Int]
    ifte_test_run = run . makeChoiceA $ ifte_test


-- called reflect in the LogicT paper
case_NdetEff_reflect :: Assertion
case_NdetEff_reflect =
  let tsplitr10 = run $ runListWriter $ makeChoiceA tsplit
      tsplitr11 = run $ runListWriter $ makeChoiceA (msplit tsplit >>= unmsplit)
      tsplitr20 = run $ makeChoiceA $ runListWriter tsplit
      tsplitr21 = run $ makeChoiceA $ runListWriter (msplit tsplit >>= unmsplit)
  in
    assertEqual "tsplitr10" expected1 tsplitr10
    >> assertEqual "tsplitr11" expected1 tsplitr11
    >> assertEqual "tsplitr20" expected2 tsplitr20
    >> assertEqual "tsplitr21" expected21 tsplitr21
  where
    expected1 = ([1, 2],["begin", "end"])
    expected2 = [(1, ["begin"]), (2, ["end"])]
    expected21 = [(1, ["begin"]), (2, ["begin", "end"])]

    unmsplit :: Member NdetEff r => (Maybe (a, Eff r a)) -> Eff r a
    unmsplit Nothing      = mzero
    unmsplit (Just (a,m)) = return a `mplus` m

    tsplit =
      (tell "begin" >> return 1) `mplus`
      (tell "end"   >> return 2)
