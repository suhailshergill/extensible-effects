-- The simplest/silliest of all benchmarks!

import Criterion.Main
import Control.Eff as E
import Control.Eff.State.Strict as E.S
import Control.Monad

-- For comparison
-- We use a strict State monad, because of large space leaks with the
-- lazy monad (one test even overflows the stack)
import Control.Monad.State.Strict as S
-- import Control.Monad.Error  as Er
-- import Control.Monad.Reader as Rd
-- import Control.Monad.Cont as Ct

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

main :: IO ()
main = defaultMain [
  bgroup "state" [ bench "State 1k" $ whnf benchCnt_State 1000
                 , bench "Eff 1k" $ whnf benchCnt_Eff 1000
                 , bench "State 10k" $ whnf benchCnt_State 10000
                 , bench "Eff 10k" $ whnf benchCnt_Eff 10000
                 ]
  ]
