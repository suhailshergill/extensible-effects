{-# LANGUAGE FlexibleContexts #-}

-- The simplest/silliest of all benchmarks!

import Criterion.Main
import Control.Eff as E
import Control.Eff.Exception as E.Er
import Control.Eff.State.Strict as E.S
import Control.Monad

-- For comparison
-- We use a strict State monad, because of large space leaks with the
-- lazy monad (one test even overflows the stack)
import Control.Monad.State.Strict as S
import Control.Monad.Error  as Er
-- import Control.Monad.Reader as Rd
-- import Control.Monad.Cont as Ct

main :: IO ()
main = defaultMain [
  bgroup "state" [ bgroup "1k" [ bench "mtl" $ whnf benchCnt_State 1000
                                , bench "eff" $ whnf benchCnt_Eff 1000
                                ]
                 , bgroup "10k" [ bench "mtl" $ whnf benchCnt_State 10000
                                , bench "eff" $ whnf benchCnt_Eff 10000
                                ]
                 ]
  , bgroup "error" [ bgroup "10k" [ bench "mtl" $ whnf benchMul_Error 10000
                                  , bench "eff" $ whnf benchMul_Eff 10000
                                  ]
                   , bgroup "100k" [ bench "mtl" $ whnf benchMul_Error 100000
                                   , bench "eff" $ whnf benchMul_Eff 100000
                                   ]
                   ]
  ]

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
benchCnt_Eff n = run $ E.S.runState m n
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

mainMul_pure = print . product $ be_make_list 1000000

instance Error Int where

benchMul_Error :: Int -> Int
benchMul_Error n = either id id m
 where
 m = foldM f 1 (be_make_list n)
 f acc 0 = Er.throwError 0
 f acc x = return $! acc * x

mainMul_Error = print $ benchMul_Error 1000000
-- 0
-- (1.39 secs, 584028840 bytes)

benchMul_Eff :: Int -> Int
benchMul_Eff n = either id id . run . runExc $ m
 where
 m = foldM f 1 (be_make_list n)
 f acc 0 = E.Er.throwExc (0::Int)
 f acc x = return $! acc * x

mainMul_Eff = print $ benchMul_Eff 1000000
-- 0
-- (1.09 secs, 519988392 bytes)
