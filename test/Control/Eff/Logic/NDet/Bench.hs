{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

-- A benchmark of shift/reset: Filinski's representing non-determinism monads
--
--  The benchmark is taken from Sec 6.1 of
--    Martin Gasbichler, Michael Sperber: Final Shift for Call/cc: Direct
--    Implementation of Shift and Reset, ICFP'02, pp. 271-282. 
--    http://www-pu.informatik.uni-tuebingen.de/users/sperber/papers/shift-reset-direct.pdf
-- This code is a straightforward translation of bench_nondet.ml
--
-- This is a micro-benchmark: it is very non-determinism-intensive. It is
-- *not* representative: the benchmark does nothing else but
-- concatenates lists. The List monad does this directly; whereas
-- continuation monads do the concatenation with more overhead (e.g.,
-- building the closures representing continuations). Therefore,
-- the List monad here outperforms all other implementations of 
-- non-determinism.
-- It should be stressed that the delimited control is optimized
-- for the case where control operations are infrequent, so we pay
-- as we go. The use of the delimited control operators is more
-- expensive, but the code that does not use delimited control does not
-- have to pay anything for delimited control. 
-- Again, in the present micro-benchmark, there is hardly any code that
-- does not use non-determinism, so the overhead of delimited control
-- is very noticeable. That is why this benchmark is good at estimating
-- the overhead of different implementations of delimited control.

-- To compile this code
-- ghc -O2 -rtsopts -main-is Bench_nondet.main_list5 Bench_nondet.hs
-- To run this code
-- GHCRTS="-tstderr" /usr/bin/time ./Bench_nondet

module Control.Eff.Logic.NDet.Bench where

import Control.Eff
import qualified Control.Eff.Logic.NDet as E

import Data.List (sort)
-- import Control.Monad.Identity
-- import Control.Monad (liftM2)
import Control.Monad (MonadPlus(..), msum)
import Control.Applicative
-- import System.CPUTime

-- Small language with non-determinism: just like the one in our DSL-WC paper

int :: MonadPlus repr => Int -> repr Int
int x = return x

add :: MonadPlus repr => repr Int -> repr Int -> repr Int
-- add xs ys = liftM2 (+) xs ys
add xs ys = do {x <- xs; y <- ys; return $! x+y }

lam :: MonadPlus repr => (repr a -> repr b) -> repr (a -> repr b)
lam f = return $ f . return

app :: MonadPlus repr => repr (a -> repr b) -> (repr a -> repr b)
app xs ys = do {x <- xs; y <- ys; x y}

amb :: MonadPlus repr => [repr Int] -> repr Int
amb = msum

-- Benchmark cases

test_ww :: MonadPlus repr => repr Int
test_ww = 
 let f = lam (\x ->
              add (add x (amb [int 6, int 4, int 2, int 8])) 
                         (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32]

ww_answer = 
 sort [8, 10, 11, 10, 7, 6, 8, 9, 8, 5, 4, 6, 7, 6, 3, 10, 12, 13,
       12, 9, 10, 12, 13, 12, 9, 8, 10, 11, 10, 7, 6, 8, 9, 8, 5, 12, 14, 15,
       14, 11, 11, 13, 14, 13, 10, 9, 11, 12, 11, 8, 7, 9, 10, 9, 6, 13, 15,
       16, 15, 12, 12, 14, 15, 14, 11, 10, 12, 13, 12, 9, 8, 10, 11, 10, 7,
       14, 16, 17, 16, 13, 13, 15, 16, 15, 12, 11, 13, 14, 13, 10, 9, 11, 12,
       11, 8, 15, 17, 18, 17, 14, 40, 42, 43, 42, 39, 38, 40, 41, 40, 37, 36,
       38, 39, 38, 35, 42, 44, 45, 44, 41]

-- Real benchmark cases

test_www :: MonadPlus repr => repr Int
test_www = 
 let f = lam (\x ->
              add (add x (amb [int 6, int 4, int 2, int 8])) 
                         (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` (f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32])

test_wwww :: MonadPlus repr => repr Int
test_wwww = 
 let f = lam (\x ->
              add (add x (amb [int 6, int 4, int 2, int 8])) 
                         (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` (f `app` (f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32]))

test_w5 :: MonadPlus repr => repr Int
test_w5 = 
 let f = lam (\x ->
              add (add x (amb [int 6, int 4, int 2, int 8])) 
                         (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` (f `app` 
     (f `app` (f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32])))


-- Different implementations of our language (MonadPlus)

-- The List monad: Non-determinism monad as a list of successes

run_list :: [Int] -> [Int]
run_list = id

testl1 = (==) [101, 201, 102, 202] . run_list $
         add (amb [int 1, int 2]) (amb [int 100, int 200])

testl2 = ww_answer == sort (run_list test_ww)


-- CPS-monad, implemented by hand; it must be quite efficient therefore
-- It is a monad, not a transformer. It cannot do any other effects beside
-- the non-determinism.
newtype CPS a = CPS{unCPS:: (a -> [Int]) -> [Int]}

instance Functor CPS where
  fmap f fa = CPS $ \k -> unCPS fa (k . f)
instance Applicative CPS where
  pure x = CPS $ \k -> k x
  mf <*> fa = CPS $ \k -> unCPS mf (\f -> unCPS fa (k . f))
instance Monad CPS where
  return x = CPS $ \k -> k x
  m >>= f  = CPS $ \k -> unCPS m (\a -> unCPS (f a) k)

instance Alternative CPS where
  empty = mzero
  (<|>) = mplus
instance MonadPlus CPS where
  mzero = CPS $ \_ -> []
  mplus m1 m2 = CPS $ \k -> unCPS m1 k ++ unCPS m2 k

run_cps :: CPS Int -> [Int]
run_cps m = unCPS m (\x -> [x])


testc1 = (==) [101, 201, 102, 202] . run_cps $
         add (amb [int 1, int 2]) (amb [int 100, int 200])

testc2 = ww_answer == sort (run_cps test_ww)

-- ExtEff implementation
-- Eff is already an instance of MonadPlus. Thus we only need to
-- define the run instance

-- run_eff :: Eff '[E.Choose] Int -> [Int]
-- run_eff = run . E.makeChoice

-- More direct interpreter
-- makeChoiceA :: Eff (E.NDet ': r) a -> Eff r [a]
-- makeChoiceA = handle_relay (\x -> x `seq` return [x] ) $ \m k -> case m of
--     E.MZero -> return []
--     E.MPlus -> liftM2 (++) (k True) (k False)

run_eff :: Eff '[E.NDet] Int -> [Int]
run_eff = run . E.makeChoiceA

teste2 = ww_answer == sort (run_eff test_ww)


data Count a = Count (Maybe a) !Int
instance Functor Count where
  fmap f (Count (Just x) n) = Count (Just (f x)) n
  fmap _ _                  = Count Nothing 0
  
instance Applicative Count where
  pure x = Count (Just x) 1
  Count (Just f) nf <*> Count (Just x) nx = Count (Just (f x)) (nf + nx)
  _ <*> _  = Count Nothing 0
  
instance Alternative Count where
  empty = Count Nothing 0
  Count m1@Just{} n1 <|> Count _ n2 = Count m1 (n1+n2)
  _ <|> m2 = m2

run_effc :: Eff '[E.NDet] Int -> Int
run_effc m = let Count _ n = run . E.makeChoiceA $ m in n

  
teste12 = length ww_answer == run_effc test_ww

{-
-- CCEx monad
-- Not a very optimal implementation of mplus (a tree would be better)
-- But is suffices as a benchmark of different implementations of CC
instance Monad m => MonadPlus (CC (PS [Int]) m) where
    mzero = abortP ps (return [])
    mplus m1 m2 = takeSubCont ps (\k ->
                     liftM2 (++)
                       (pushPrompt ps (pushSubCont k m1))
                       (pushPrompt ps (pushSubCont k m2)))

run_dir :: CC (PS [Int]) Identity Int -> [Int]
run_dir m = runIdentity . runCC $
            pushPrompt ps (m >>= return . (:[]))


testd1 = (==) [101, 201, 102, 202] . run_dir $
         add (amb [int 1, int 2]) (amb [int 100, int 200])

testd2 = ww_answer == sort (run_dir test_ww)

-}


-- Benchmarks themselves

main_list3 = print $ 2400   == (length . run_list $ test_www)
main_list4 = print $ 48000  == (length . run_list $ test_wwww)
main_list5 = print $ 960000 == (length . run_list $ test_w5)

main_cps3 = print $ 2400   == (length . run_cps $ test_www)
main_cps4 = print $ 48000  == (length . run_cps $ test_wwww)
main_cps5 = print $ 960000 == (length . run_cps $ test_w5)

-- We expect the direct implementation to be slower since CC is the transformer,
-- whereas CPS is not. The latter is hand-written for a specific answer-type.
main_eff3 = print $ 2400   == (length . run_eff $ test_www)
main_eff4 = print $ 48000  == (length . run_eff $ test_wwww)
main_eff5 = print $ 960000 == (length . run_eff $ test_w5)

main_eff5c = print $ 960000 == (run_effc $ test_w5)

-- To clarify the effect of building a list
main_eff5m = print $ ((run . E.makeChoiceA $ test_w5) :: Maybe Int)

{-
-- Instantiate CC to the IO as the base monad, attempting to quantify the
-- effect of the Identity transformer
main_dir5io = do
              l <- runCC $ pushPrompt ps (test_w5 >>= return . (:[]))
              print $ length l == 960000
-}

-- ------------------------------------------------------------------------
-- Old results, from 2010

{- Median of 5 runs

main_list5
<<ghc: 186526764 bytes, 356 GCs, 619182/1156760 avg/max bytes residency (3 samples), 4M in use, 0.00 INIT (0.00 elapsed), 0.25 MUT (0.25 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
        0.30 real         0.30 user         0.00 sys

main_cps5
<<ghc: 231580040 bytes, 442 GCs, 4017/4104 avg/max bytes residency (24 samples), 2M in use, 0.00 INIT (0.00 elapsed), 0.28 MUT (0.28 elapsed), 0.31 GC (0.33 elapsed) :ghc>>
        0.60 real         0.58 user         0.01 sys

main_dir5 (CCExc implementation)
<<ghc: 780415108 bytes, 1489 GCs, 10459973/39033060 avg/max bytes residency (14 samples), 110M in use, 0.00 INIT (0.00 elapsed), 1.30 MUT (1.32 elapsed), 2.92 GC (3.14 elapsed) :ghc>>
        4.48 real         4.22 user         0.24 sys

main_dir5io (CCExc implementation)
<<ghc: 1148031880 bytes, 2190 GCs, 10339954/38941944 avg/max bytes residency (14 samples), 108M in use, 0.00 INIT (0.00 elapsed), 2.15 MUT (2.20 elapsed), 3.04 GC (3.24 elapsed) :ghc>>
        5.45 real         5.18 user         0.21 sys


main_dir5 (CCCxe implementation)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 991065016 bytes, 1891 GCs, 10473968/38790660 avg/max bytes residency (14 samples), 110M in use, 0.00 INIT (0.00 elapsed), 1.45 MUT (1.49 elapsed), 2.99 GC (3.20 elapsed) :ghc>>
        4.70 real         4.44 user         0.23 sys

main_dir5io (CCCxe implementation)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 991065412 bytes, 1891 GCs, 10364029/37920012 avg/max bytes residency (14 samples), 109M in use, 0.00 INIT (0.00 elapsed), 1.46 MUT (1.50 elapsed), 2.99 GC (3.20 elapsed) :ghc>>
        4.72 real         4.44 user         0.23 sys

main_ref5io (without pushDelimSubCont)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 19050261764 bytes, 36337 GCs, 10620542/49328200 avg/max bytes residency (16 samples), 123M in use, 0.00 INIT (0.00 elapsed), 61.45 MUT (62.70 elapsed), 6.06 GC (6.21 elapsed) :ghc>>
       68.94 real        67.51 user         1.03 sys


main_ref5io (with pushDelimSubCont)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 5666546308 bytes, 10809 GCs, 10538302/46414760 avg/max bytes residency (14 samples), 114M in use, 0.00 INIT (0.00 elapsed), 16.27 MUT (16.68 elapsed), 3.65 GC (3.80 elapsed) :ghc>>
       20.50 real        19.92 user         0.46 sys

-}

-- ------------------------------------------------------------------------
-- Newer Benchmarks, July 2015

{-
main_list5
True
<<ghc: 374751856 bytes, 720 GCs, 939265/2386984 avg/max bytes residency (6 samples), 7M in use, 0.00 INIT (0.00 elapsed), 0.11 MUT (0.11 elapsed), 0.02 GC (0.02 elapsed) :ghc>>

main_cps5
True
<<ghc: 463450920 bytes, 889 GCs, 36708/44312 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.14 MUT (0.15 elapsed), 0.00 GC (0.01 elapsed) :ghc>>

-- using makeChoiceA (setting f as an Alternative)
main_eff5
True
<<ghc: 1013337072 bytes, 1944 GCs, 18671465/83300976 avg/max bytes residency (17 samples), 231M in use, 0.00 INIT (0.00 elapsed), 0.36 MUT (0.39 elapsed), 1.08 GC (1.13 elapsed) :ghc>>

With strict add:
True
<<ghc: 993935088 bytes, 1906 GCs, 15000238/77154800 avg/max bytes residency (19 samples), 199M in use, 0.00 INIT (0.00 elapsed), 0.37 MUT (0.39 elapsed), 0.95 GC (1.02 elapsed) :ghc>>
1.32user 0.08system 0:01.40elapsed 99%CPU (0avgtext+0avgdata 819408maxresident)k
0inputs+0outputs (0major+51485minor)pagefaults 0swaps

It looks like a huge memory leak. Perhaps the list is fully realized?


Using the counting Alternative Count
True
<<ghc: 591341472 bytes, 1133 GCs, 16603280/76447176 avg/max bytes residency (10 samples), 162M in use, 0.00 INIT (0.00 elapsed), 0.28 MUT (0.28 elapsed), 0.61 GC (0.66 elapsed) :ghc>>

Using Maybe
Just 32
<<ghc: 523838824 bytes, 1003 GCs, 16969712/76447176 avg/max bytes residency (9 samples), 150M in use, 0.00 INIT (0.00 elapsed), 0.21 MUT (0.19 elapsed), 0.46 GC (0.52 elapsed) :ghc>>
0.67user 0.05system 0:00.72elapsed 100%CPU (0avgtext+0avgdata 620752maxresident)k
0inputs+0outputs (0major+38937minor)pagefaults 0swaps

-- using Maybe, but with the better makeChoice
Just 32
<<ghc: 517460016 bytes, 883 GCs, 20215861/91552144 avg/max bytes residency (9 samples), 138M in use, 0.00 INIT (0.00 elapsed), 0.22 MUT (0.24 elapsed), 0.41 GC (0.43 elapsed) :ghc>>
0.63user 0.04system 0:00.68elapsed 100%CPU (0avgtext+0avgdata 570720maxresident)k
0inputs+0outputs (0major+35760minor)pagefaults 0swaps

Better makeChoiceA, full list
True
<<ghc: 454475112 bytes, 839 GCs, 8700298/33304904 avg/max bytes residency (8 samples), 58M in use, 0.00 INIT (0.00 elapsed), 0.23 MUT (0.23 elapsed), 0.19 GC (0.20 elapsed) :ghc>>
0.42user 0.02system 0:00.44elapsed 100%CPU (0avgtext+0avgdata 244064maxresident)k
0inputs+0outputs (0major+15391minor)pagefaults 0swaps

-}
