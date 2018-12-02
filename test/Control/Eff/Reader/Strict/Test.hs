{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Reader.Strict.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Reader.Strict
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Strict1_Reader_runReader :: Assertion
case_Strict1_Reader_runReader = let
  e = run $ runReader (undefined :: ()) voidReader
  in
   assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (ask :: Eff '[Reader ()] ())
        return ()

case_Strict1_Reader_monadBaseControl :: Assertion
case_Strict1_Reader_monadBaseControl =
      runLift (runReader i act) @=? (Just i)
    where
        act = doThing ask
        i = 10 :: Int
