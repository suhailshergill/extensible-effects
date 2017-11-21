{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing #-}

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
