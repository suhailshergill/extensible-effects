{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing #-}

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
  e = run $ runReader voidReader (undefined :: ())
  in
   assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (ask :: Eff '[Reader ()] ())
        return ()
