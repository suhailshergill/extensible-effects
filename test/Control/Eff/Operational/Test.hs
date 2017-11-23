{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Operational.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Operational
import Control.Eff.Operational.Example as Eg
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Operational_Monad :: Assertion
case_Operational_Monad =
  let comp :: (Member (State [String]) r
               , Member (Writer String) r)
              => Eff r ()
      comp = runProgram Eg.adventPure Eg.prog
      go = snd . run . runMonoidWriter $ evalState comp ["foo", "bar"]
  in
   assertEqual
   "Evaluating Operational Monad example"
   (unlines ["getting input...",
             "ok",
             "the input is foo"]) go
