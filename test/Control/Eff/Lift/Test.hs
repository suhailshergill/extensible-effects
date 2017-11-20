{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Lift.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Data.OpenUnion
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Monad m, SetMember Lift (Lift m) r) => Eff r ()
    possiblyAmbiguous = lift $ return ()

case_Lift_tl1r :: Assertion
case_Lift_tl1r = do
  ((), output) <- catchOutput tl1r
  assertEqual "Test tl1r" (showLn input) output
  where
    input = (5::Int)
    -- tl1r :: IO ()
    tl1r = runLift (runReader tl1 input)
      where
        tl1 = ask >>= \(x::Int) -> lift . print $ x

case_Lift_tMd' :: Assertion
case_Lift_tMd' = do
  actual <- catchOutput tMd'
  let expected = (output, (showLines input))
  assertEqual "Test mapMdebug using Lift" expected actual
  where
    input = [1..5]
    val = (10::Int)
    output = map (+ val) input

    tMd' = runLift $ runReader (mapMdebug' f input) val
      where f x = ask `add` return x

    -- Re-implemenation of mapMdebug using Lifting
    -- The signature is inferred
    mapMdebug'  :: (Show a, SetMember Lift (Lift IO) r) =>
                   (a -> Eff r b) -> [a] -> Eff r [b]
    mapMdebug' f [] = return []
    mapMdebug' f (h:t) = do
      lift $ print h
      h' <- f h
      t' <- mapMdebug' f t
      return (h':t')
