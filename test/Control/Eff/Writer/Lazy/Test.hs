{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Writer.Lazy.Test (testGroups) where

import Test.HUnit hiding (State)
import Test.QuickCheck

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.Writer.Lazy
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

testGroups = [ $(testGroupGenerator) ]

addGet :: Member (Reader Int) r  => Int -> Eff r Int
addGet x = ask >>= \i -> return (i+x)

addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

case_Lazy1_Writer_rdwr :: Assertion
case_Lazy1_Writer_rdwr = (10, ["begin", "end"]) @=?
  (run . runReader (1::Int) . runListWriter $ rdwr)
  where
    rdwr = do
      tell "begin"
      r <- addN 10
      tell "end"
      return r

prop_Lazy1_Writer_censor :: [Integer] -> Property
prop_Lazy1_Writer_censor l =
  property
  $ listE (mapM_ (tell . inc) l) == listE (censor inc $ mapM_ tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: Eff '[Writer Integer] () -> [Integer]
    listE = snd . run . runListWriter

case_Lazy1_Writer_runFirstWriter :: Assertion
case_Lazy1_Writer_runFirstWriter = let
  ((), Just m) = run $ runFirstWriter $ mapM_ tell [(), undefined]
  in
   assertNoUndefined (m :: ())

case_Lazy1_Writer_runLastWriter :: Assertion
case_Lazy1_Writer_runLastWriter = let
  ((), Just m) = run $ runLastWriter $ mapM_ tell [undefined, ()]
  in
   assertNoUndefined (m :: ())

case_Lazy1_Writer_monadBaseControl :: Assertion
case_Lazy1_Writer_monadBaseControl = runLift (runListWriter act) @=? Just ((), [i])
  where
    i = 10 :: Int
    act = doThing (tell i)
