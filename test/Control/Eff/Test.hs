{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing #-}

module Control.Eff.Test (testGroups) where

import Test.QuickCheck
import Control.Eff
import Control.Eff.Reader.Strict

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

testGroups = [ $(testGroupGenerator) ]

prop_NestedEff :: Property
prop_NestedEff = forAll arbitrary (\x -> property (qu x == x))
  where
    qu :: Bool -> Bool
    qu x = run $ runReader (readerAp x) readerId

    readerAp :: Bool -> Eff '[Reader (Eff '[Reader Bool] Bool)] Bool
    readerAp x = do
      f <- ask
      return . run $ runReader f x

    readerId :: Eff '[Reader Bool] Bool
    readerId = do
      x <- ask
      return x
