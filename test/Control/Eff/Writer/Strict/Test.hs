{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Writer.Strict.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Writer.Strict
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Strict1_Writer_runLastWriter :: Assertion
case_Strict1_Writer_runLastWriter = let
  ((), Just m) = run $ runLastWriter $ mapM_ tell [undefined, ()]
  in
   assertUndefined (m :: ())

case_Strict1_Writer_monadBaseControl :: Assertion
case_Strict1_Writer_monadBaseControl = runLift (runListWriter act) @=? Just ((), [i])
  where
    i = 10 :: Int
    act = doThing (tell i)
