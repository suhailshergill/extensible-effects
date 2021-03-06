{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Example.Fresh.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Example.Fresh
import Control.Eff.Trace
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Fresh_tfresh' :: Assertion
case_Fresh_tfresh' = do
  ((), actual) <- catchOutput tfresh'
  assertOutput "Fresh: test"
    ["Fresh 0", "Fresh 1"] actual
  where
    tfresh' = runTrace $ runFresh' 0 $ do
      n <- fresh
      trace $ "Fresh " ++ show n
      n <- fresh
      trace $ "Fresh " ++ show n

case_Fresh_monadBaseControl :: Assertion
case_Fresh_monadBaseControl = runLift (runFresh' i (doThing $ fresh >> fresh)) @=? Just (i + 1)
  where
    i = 0
