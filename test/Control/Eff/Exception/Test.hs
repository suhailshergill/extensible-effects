{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Exception.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Writer.Strict
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

-- The type is inferred
-- et1 :: Eff r Int
et1 = return 1 `add` return 2

case_Exception1_et1 :: Assertion
case_Exception1_et1 = 3 @=? (run et1)

-- The type is inferred
-- et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwError (2::Int)

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    Could not deduce (Data.OpenUnion.FindElem (Exc Int) '[])
      arising from a use of `et2'
-}

case_Exception1_et21 :: Assertion
case_Exception1_et21 = (Left (2::Int)) @=?
  (run et21)
  where
    -- The inferred type shows that ex21 is now pure
    -- et21 :: Eff r (Either Int Int)

    et21 = runError et2

-- Implementing the operator <|> from Alternative:
--  a <|> b does
--   -- tries a, and if succeeds, returns its result
--   -- otherwise, tries b, and if succeeds, returns its result
--   -- otherwise, throws mappend of exceptions of a and b

-- We use SetMember in the signature rather than Member to
-- ensure that the computation throws only one type of exceptions.
-- Otherwise, this construction is not very useful.
alttry :: forall e r a. (Monoid e, SetMember Exc (Exc e) r) =>
          Eff r a -> Eff r a -> Eff r a
alttry ma mb =
  catchError ma $ \ea ->
  catchError mb $ \eb -> throwError (mappend (ea::e) eb)

case_Exception1_alttry :: Assertion
case_Exception1_alttry =
  [Right 10,Right 10,Right 10,Left "bummer1bummer2"] @=?
  [
  run . runError $
     (return 1 `add` throwError "bummer1") `alttry`
     (return 10),
  run . runError $
     (return 10) `alttry`
     (return 1 `add` throwError "bummer2"),
  run . runError $
     (return 10) `alttry` return 20,
  run . runError $
     (return 1 `add` throwError "bummer1") `alttry`
     (return 1 `add` throwError "bummer2")
     ]

case_Failure1_Effect :: Assertion
case_Failure1_Effect =
  let go :: Eff (Exc () ': Writer Int ': '[]) Int
         -> Int
      go = snd . run . runWriter (+) 0 . ignoreFail
      ret = go $ do
        tell (1 :: Int)
        tell (2 :: Int)
        tell (3 :: Int)
        () <- die
        tell (4 :: Int)
        return 5
   in assertEqual "Fail should stop writing" 6 ret
