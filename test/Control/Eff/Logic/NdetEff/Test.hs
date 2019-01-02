{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Logic.NdetEff.Test (testGroups, gen_testCA, gen_ifte_test)
where

import Test.HUnit hiding (State)
import Control.Applicative
import Control.Eff
import Control.Eff.Example
import Control.Eff.Example.Test (ex2)
import Control.Eff.Exception
import Control.Eff.Logic.NdetEff
import Control.Eff.Writer.Strict
import Control.Monad (msum, guard, mzero, mplus)
import Control.Eff.Logic.Test
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

-- TODO: add quickcheck test to test conformance of different
-- implementations of 'makeChoiceA' and 'msplit'.

-- TODO: add benchmarks for different implementations of 'makeChoiceA'
-- and 'msplit'.

gen_testCA :: (Integral a) => a -> Eff (NdetEff ': r) a
gen_testCA x = do
  i <- msum . fmap return $ [1..x]
  guard (i `mod` 2 == 0)
  return i

case_NdetEff_testCA :: Assertion
case_NdetEff_testCA = [2, 4..10] @=? (run $ makeChoiceA (gen_testCA 10))

case_Choose1_exc11 :: Assertion
case_Choose1_exc11 = [2,3] @=? (run exc11)
  where
    exc11 = makeChoice exc1
    exc1 = return 1 `add` choose [1,2]

case_Choose_exRec :: Assertion
case_Choose_exRec =
  let exRec_1 = run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,1]))
      exRec_2 = run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,1]))
      exRec_3 = run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,11,1]))
      exRec_4 = run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,11,1]))
  in
    assertEqual "Choose: error recovery: exRec_1" expected1 exRec_1
    >> assertEqual "Choose: error recovery: exRec_2" expected2 exRec_2
    >> assertEqual "Choose: error recovery: exRec_3" expected3 exRec_3
    >> assertEqual "Choose: error recovery: exRec_4" expected4 exRec_4
  where
    expected1 = Right [5,7,1]
    expected2 = [Right 5,Right 7,Right 1]
    expected3 = Left (TooBig 11)
    expected4 = [Right 5,Right 7,Left (TooBig 11),Right 1]
    -- Errror recovery part
    -- The code is the same as in transf1.hs. The inferred signatures differ
    -- Was: exRec :: MonadError TooBig m => m Int -> m Int
    -- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
    exRec m = catchError m handler
      where handler (TooBig n) | n <= 7 = return n
            handler e = throwError e

case_Choose_ex2 :: Assertion
case_Choose_ex2 =
  let ex2_1 = run . makeChoice . runErrBig $ ex2 (choose [5,7,1])
      ex2_2 = run . runErrBig . makeChoice $ ex2 (choose [5,7,1])
  in
    assertEqual "Choose: Combining exceptions and non-determinism: ex2_1"
    expected1 ex2_1
    >> assertEqual "Choose: Combining exceptions and non-determinism: ex2_2"
    expected2 ex2_2
  where
    expected1 = [Right 5,Left (TooBig 7),Right 1]
    expected2 = Left (TooBig 7)

gen_ifte_test x = do
  n <- gen x
  ifte (do
           d <- gen x
           guard $ d < n && n `mod` d == 0
           -- _ <- trace ("d: " ++ show d) (return ())
       )
    (\_ -> mzero)
    (return n)
    where gen x = msum . fmap return $ [2..x]


case_NdetEff_ifte :: Assertion
case_NdetEff_ifte =
  let primes = ifte_test_run
  in
    assertEqual "NdetEff: test ifte using primes"
    [2,3,5,7,11,13,17,19,23,29] primes
  where
    ifte_test_run :: [Int]
    ifte_test_run = run . makeChoiceA $ (gen_ifte_test 30)


-- called reflect in the LogicT paper
case_NdetEff_reflect :: Assertion
case_NdetEff_reflect =
  let tsplitr10 = run $ runListWriter $ makeChoiceA tsplit
      tsplitr11 = run $ runListWriter $ makeChoiceA (msplit tsplit >>= reflect)
      tsplitr20 = run $ makeChoiceA $ runListWriter tsplit
      tsplitr21 = run $ makeChoiceA $ runListWriter (msplit tsplit >>= reflect)
  in
    assertEqual "tsplitr10" expected1 tsplitr10
    >> assertEqual "tsplitr11" expected1 tsplitr11
    >> assertEqual "tsplitr20" expected2 tsplitr20
    >> assertEqual "tsplitr21" expected21 tsplitr21
  where
    expected1 = ([1, 2],["begin", "end"])
    expected2 = [(1, ["begin"]), (2, ["end"])]
    expected21 = [(1, ["begin"]), (2, ["begin", "end"])]

    tsplit =
      (tell "begin" >> return 1) `mplus`
      (tell "end"   >> return 2)

case_NdetEff_monadBaseControl :: Assertion
case_NdetEff_monadBaseControl = runLift (makeChoiceA $ doThing (return 1 <|> return 2)) @=? Just [1,2]

case_Choose_monadBaseControl :: Assertion
case_Choose_monadBaseControl = runLift (makeChoice $ doThing $ choose [1,2,3]) @=? Just [1,2,3]

case_NdetEff_cut :: Assertion
case_NdetEff_cut = testCut (run . makeChoice)

case_NdetEff_monadplus :: Assertion
case_NdetEff_monadplus =
  let evalnw = run . (runListWriter @Int) . makeChoice
      evalwn = run . makeChoice . (runListWriter @Int)
      casesnw = [
        -- mplus laws
          ("0             | NdetEff, Writer", evalnw t0, nw0)
        , ("zm0     = 0   | NdetEff, Writer", evalnw tzm0, nw0)
        , ("0m1           | NdetEff, Writer", evalnw t0m1, nw0m1)
        , ("zm0mzm1 = 0m1 | NdetEff, Writer", evalnw tzm0mzm1, nw0m1)
        -- mzero laws
        , ("z         | NdetEff, Writer", evalnw tz, nwz)
        , ("z0    = z | NdetEff, Writer", evalnw tz0, nwz)
        , ("0z   /= z | NdetEff, Writer", evalnw t0z, nw0z)
        , ("z0m1  = 1 | NdetEff, Writer", evalnw tz0m1, nw1)
        , ("0zm1 /= 1 | NdetEff, Writer", evalnw t0zm1, nw0zm1)
        ]
      caseswn = [
        -- mplus laws
          ("0             | Writer, NdetEff", evalwn t0, wn0)
        , ("zm0     = 0   | Writer, NdetEff", evalwn tzm0, wn0)
        , ("0m1           | Writer, NdetEff", evalwn t0m1, wn0m1)
        , ("zm0mzm1 = 0m1 | Writer, NdetEff", evalwn tzm0mzm1, wn0m1)
        -- mzero laws
        , ("z        | Writer, NdetEff", evalwn tz, wnz)
        , ("z0   = z | Writer, NdetEff", evalwn tz0, wnz)
        , ("0z   = z | Writer, NdetEff", evalwn t0z, wnz)
        , ("z0m1 = 1 | Writer, NdetEff", evalwn tz0m1, wn1)
        , ("0zm1 = 1 | Writer, NdetEff", evalwn t0zm1, wn1)
        ]
  in runAsserts assertEqual casesnw
  >> runAsserts assertEqual caseswn
  where
    nwz = ([]::[Int],[])
    wnz = [] ::[(Int, [Int])]
    nw0z = ([]::[Int],[0])
    nw0 = ([0],[0])
    nw1 = ([1],[1])
    nw0zm1 = ([1],[0,1])
    wn0 = [(0,[0])]
    wn1 = [(1,[1])]

    nw0m1 = ([0::Int,1],[0,1])
    wn0m1 = [(0,[0]), (1,[1])]

    t0 = wr @Int 0
    t1 = wr @Int 1

    tz = mzero
    tz0 = tz >> t0
    t0z = t0 >> tz
    tz0m1 = tz0 `mplus` t1
    t0zm1 = t0z `mplus` t1

    t0m1 = t0 `mplus` t1
    tzm0 = tz `mplus` t0
    tzm1 = tz `mplus` t1
    tzm0mzm1 = tzm0 `mplus` tzm1

    wr :: forall a r. [Writer a, NdetEff] <:: r => a -> Eff r a
    wr i = tell i >> return i
