{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Control.Eff.Reader.Lazy.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Monad
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

t1 = ask `add` return (1::Int)

case_Lazy1_Reader_t1 :: Assertion
case_Lazy1_Reader_t1 = let
  t1' = do v <- ask; return (v + 1 :: Int)
  t1r = runReader t1 (10::Int)
  in
    -- 'run t1' should result in type-error
    11 @=? (run t1r)

t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))


case_Lazy1_Reader_t2 :: Assertion
case_Lazy1_Reader_t2 = let
  t2r = runReader t2 (10::Int)
  t2rr = flip runReader (20::Float) . flip runReader (10::Int) $ t2
  in
    33.0 @=? (run t2rr)

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}
case_Lazy1_Reader_t2' :: Assertion
case_Lazy1_Reader_t2' = 33.0 @=?
  (run $ runReader (runReader t2 (20::Float)) (10::Int))


case_Lazy1_Reader_t3 :: Assertion
case_Lazy1_Reader_t3 = let
  t3 = t1 `add` local (+ (10::Int)) t1
  in
    212 @=? (run $ runReader t3 (100::Int))

-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (local (+ (10::Int)) t2)
                (local (+ (30::Float)) t2)

case_Lazy1_Reader_t4 :: Assertion
case_Lazy1_Reader_t4 = 106.0 @=?
  (run $ runReader (runReader t4 (20::Float)) (10::Int))

-- The opposite order of layers gives the same result
case_Lazy1_Reader_t4' :: Assertion
case_Lazy1_Reader_t4' = 106.0 @=?
  (run $ runReader (runReader t4 (20::Float)) (10::Int))

-- Map an effectful function
case_Lazy1_Reader_tmap :: Assertion
case_Lazy1_Reader_tmap = let
  tmap = mapM f [1..5]
  in
    ([11,12,13,14,15] :: [Int]) @=?
    (run $ runReader tmap (10::Int))
  where
    f x = ask `add` return x

case_Lazy1_Reader_runReader :: Assertion
case_Lazy1_Reader_runReader = let
  e = run $ runReader voidReader (undefined :: ())
  in
   assertNoUndefined (e :: ())
  where
    voidReader = do
        _ <- (ask :: Eff '[Reader ()] ())
        return ()

case_Lazy1_Reader_monadBaseControl :: Assertion
case_Lazy1_Reader_monadBaseControl =
      runLift (runReader act i) @=? (Just i)
    where
        act = doThing ask
        i = 10 :: Int
