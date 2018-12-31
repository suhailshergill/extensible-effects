{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- The simplest/silliest of all benchmarks!

import Criterion.Main
import Control.Eff as E
import Control.Eff.Exception as E.Er
import Control.Eff.Logic.NdetEff as E.ND
import Control.Eff.State.Strict as E.S
import Control.Monad

-- For comparison
-- We use a strict State monad, because of large space leaks with the
-- lazy monad (one test even overflows the stack)
import Control.Monad.State.Strict as S
import Control.Monad.Except  as Er
-- import Control.Monad.Reader as Rd
import Control.Monad.Cont as Ct
import Control.Applicative

-- For sanity-checking
import qualified Test.Framework as TF
import qualified Test.Framework.TH as TF.TH
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as HU

main :: IO ()
main = defaultMain [
  bgroup "state" [ bgroup "10k" [ bench "mtl" $ whnf benchCnt_State 10000
                                , bench "eff" $ whnf benchCnt_Eff 10000
                                ]
                 ]
  , bgroup "error" [ bgroup "50k" [ bench "mtl" $ whnf benchMul_Error 50000
                                  , bench "eff" $ whnf benchMul_Eff 50000
                                  ]
                   ]
  , bgroup "st-error" [ bgroup "err : st" [ bench "mtl" $ whnf mainMax_MTL 10000
                                          , bench "eff" $ whnf mainMax_Eff 10000
                                          ]
                      , bgroup "st : err" [ bench "mtl" $ whnf mainMax1_MTL 10000
                                          , bench "eff" $ whnf mainMax1_Eff 10000
                                          ]
                      ]
  , bgroup "pyth" [ bgroup "ndet" [ bench "mtl" $ whnf mainN_MTL 20
                                  , bench "eff" $ whnf mainN_Eff 20
                                  ]
                  , bgroup "ndet : st" [ bench "mtl" $ nf mainNS_MTL 15
                                       , bench "eff" $ nf mainNS_Eff 15
                                       ]
                  ]
  ]
  >> TF.defaultMainWithArgs [ $(TF.TH.testGroupGenerator) ] testOpts
  where
    testOpts = [ "--color" ]

-- ------------------------------------------------------------------------
-- Single State, with very little non-effectful computation
-- This is a micro-benchmark, and hence not particularly realistic.
-- Because of its simplicity, GHC may do a lot of inlining.
-- See a more realistic max benchmark below, which does a fair amount
-- of computation other than accessing the state.

-- Count-down
benchCnt_State :: Int -> ((),Int)
benchCnt_State n = S.runState m n
 where
 m = do
     x <- S.get
     if x > 0 then S.put (x-1) >> m else return ()

benchCnt_Eff :: Int -> ((),Int)
benchCnt_Eff n = run $ E.S.runState n m
 where
 m = do
     x <- E.S.get
     if x > 0 then E.S.put (x-1::Int) >> m else return ()

-- ------------------------------------------------------------------------
-- Single Error

-- Multiply a list of numbers, throwing an exception when encountering 0
-- This is again a mcro-benchmark

-- make a list of n ones followed by 0
be_make_list :: Int -> [Int]
be_make_list n = replicate n 1 ++ [0]

benchMul_Error :: Int -> Int
benchMul_Error n = either id id m
 where
 m = foldM f 1 (be_make_list n)
 f acc 0 = Er.throwError 0
 f acc x = return $! acc * x

benchMul_Eff :: Int -> Int
benchMul_Eff n = either id id . run . runError $ m
 where
 m = foldM f 1 (be_make_list n)
 f acc 0 = E.Er.throwError (0::Int)
 f acc x = return $! acc * x

-- ------------------------------------------------------------------------
-- State and Error and non-effectful computation

benchMax_MTL :: (MonadState Int m, MonadError Int m) => Int -> m Int
benchMax_MTL n = foldM f 1 [n, n-1 .. 0]
 where
 f acc 0 = Er.throwError 0
 f acc x | x `mod` 5 == 0 = do
                            s <- S.get
                            S.put $! (s+1)
                            return $! max acc x
 f acc x = return $! max acc x

mainMax_MTL n = S.runState (Er.runExceptT (benchMax_MTL n)) 0

-- Different order of layers
mainMax1_MTL n = (S.runStateT (benchMax_MTL n) 0 :: Either Int (Int,Int))

benchMax_Eff :: (Member (Exc Int) r, Member (E.S.State Int) r) =>
                Int -> Eff r Int
benchMax_Eff n = foldM f 1 [n, n-1 .. 0]
 where
 f acc 0 = E.Er.throwError (0::Int)
 f acc x | x `mod` 5 == 0 = do
                            s <- E.S.get
                            E.S.put $! (s+1::Int)
                            return $! max acc x
 f acc x = return $! max acc x


mainMax_Eff n = ((run $ E.S.runState 0 (E.Er.runError (benchMax_Eff n))) ::
                  (Either Int Int,Int))

mainMax1_Eff n = ((run $ E.Er.runError (E.S.runState 0 (benchMax_Eff n))) ::
                     Either Int (Int,Int))

-- ------------------------------------------------------------------------
-- Non-determinism benchmark: Pythagorian triples

-- First benchmark, with non-determinism only

-- Stream from k to n
iota k n = if k > n then mzero else return k `mplus` iota (k+1) n

pyth1 :: MonadPlus m => Int -> m (Int, Int, Int)
pyth1 upbound = do
  x <- iota 1 upbound
  y <- iota 1 upbound
  z <- iota 1 upbound
  if x*x + y*y == z*z then return (x,y,z) else mzero

pyth20 =
  [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(8,15,17),(9,12,15),(12,5,13),
   (12,9,15),(12,16,20),(15,8,17),(16,12,20)]


case_pythr_ndet :: HU.Assertion
case_pythr_ndet =
  HU.assertEqual "pythr_MTL" pyth20 ((runCont (pyth1 20) (\x -> [x])) :: [(Int,Int,Int)])
  >> HU.assertEqual "pythr_EFF" pyth20 ((run . E.ND.makeChoiceLst $ pyth1 20) :: [(Int,Int,Int)])


-- There is no instance of MonadPlus for ContT
-- we have to make our own

instance Monad m => MonadPlus (ContT [r] m) where
  mzero = ContT $ \k -> return []
  mplus (ContT m1) (ContT m2) = ContT $ \k ->
    liftM2 (++) (m1 k) (m2 k)

instance Monad m => Alternative (ContT [r] m) where
  empty = mzero
  (<|>) = mplus

mainN_MTL n = ((runCont (pyth1 n) (\x -> [x])) :: [(Int,Int,Int)])

mainN_Eff n = ((run . E.ND.makeChoiceLst $ pyth1 n) :: [(Int,Int,Int)])

-- Adding state: counting the number of choices

pyth2 :: Int -> ContT [r] (S.State Int) (Int, Int, Int)
pyth2 upbound = do
  x <- iota 1 upbound
  y <- iota 1 upbound
  z <- iota 1 upbound
  cnt <- S.get
  S.put $! (cnt + 1)
  if x*x + y*y == z*z then return (x,y,z) else mzero

pyth2E :: (Member (E.S.State Int) r, Member NdetEff r) =>
          Int -> Eff r (Int, Int, Int)
pyth2E upbound = do
  x <- iota 1 upbound
  y <- iota 1 upbound
  z <- iota 1 upbound
  cnt <- E.S.get
  E.S.put $! (cnt + 1::Int)
  if x*x + y*y == z*z then return (x,y,z) else mzero


mainNS_MTL n =
  let (l,cnt) = pythrNS_MTL n
  in ((l::[(Int,Int,Int)]), (cnt::Int))
  where
    pythrNS_MTL :: Int -> ([(Int,Int,Int)],Int)
    pythrNS_MTL n = S.runState (runContT (pyth2 n) (\x -> return [x])) 0

mainNS_Eff n =
  let (l,cnt) = pyth2Er n
  in ((l::[(Int,Int,Int)]), (cnt::Int))
  where
    pyth2Er :: Int -> ([(Int,Int,Int)],Int)
    pyth2Er n = run . E.S.runState 0 . E.ND.makeChoiceLst $ pyth2E n
