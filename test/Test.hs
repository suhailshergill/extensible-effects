{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Control.Exception (ErrorCall, catch)
import Data.Typeable

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import Test.HUnit hiding (State)
import Test.QuickCheck

import Control.Eff
import Data.OpenUnion
import qualified Control.Eff.Reader.Lazy as LazyR
import qualified Control.Eff.Reader.Strict as StrictR
import qualified Control.Eff.Writer.Lazy as LazyW
import qualified Control.Eff.Writer.Strict as StrictW
import qualified Control.Eff.State.Strict as StrictS
import qualified Control.Eff.State.Lazy as LazyS
import Control.Eff.Exception
import Control.Eff.Choose as Choose
import Control.Eff.NdetEff
import Control.Monad (liftM2, msum, guard, mzero, mplus)
import Data.Monoid

import Control.Eff.Example as Eg
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.Operational as Op
import Control.Eff.Operational.Example as Op.Eg
import Control.Eff.Trace
import Control.Eff.Coroutine
import Data.Void

-- {{{ utils: TODO: move them out

import GHC.IO.Handle
import System.IO
import System.Directory

-- | capture stdout
-- [[https://stackoverflow.com/a/9664017][source]]
catchOutput :: IO a -> IO (a, String)
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  fVal <- f
  hDuplicateTo stdout_dup stdout
  str <- readFile tmpf
  removeFile tmpf
  return (fVal, str)

showLn :: Show a => a -> String
showLn x = unlines $ [show x]

showLines :: Show a => [a] -> String
showLines xs = unlines $ map show xs

withError :: a -> ErrorCall -> a
withError a _ = a

assertUndefined :: a -> Assertion
assertUndefined a = catch (seq a $ assertFailure "") (withError $ return ())

assertNoUndefined :: a -> Assertion
assertNoUndefined a = catch (seq a $ return ()) (withError $ assertFailure "")

allEqual :: Eq a => [a] -> Bool
allEqual = all (uncurry (==)) . pairs
  where
    pairs l = zip l $ tail l

safeLast [] = Nothing
safeLast l = Just $ last l

-- }}}

main :: IO ()
main = defaultMain tests

tests = [
  $(testGroupGenerator)
#if __GLASGOW_HASKELL__ >= 708
  , testProperty "Test nested Eff." testNestedEff
#endif
        ]

-- {{{ Documentation example

prop_Documentation_example :: [Integer] -> Property
prop_Documentation_example l = let
  ((), total1) = run $ LazyS.runState (Eg.sumAll l) 0
  ((), last1) = run $ LazyW.runLastWriter $ Eg.writeAll l
  (((), last2), total2) = run $ LazyS.runState (LazyW.runLastWriter (Eg.writeAndAdd l)) 0
  (((), total3), last3) = run $ LazyW.runLastWriter $ LazyS.runState (Eg.writeAndAdd l) 0
  in
   allEqual [safeLast l, last1, last2, last3]
   .&&. allEqual [sum l, total1, total2, total3]

-- }}}

-- {{{ Reader

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)
t1 = LazyR.ask `add` return (1::Int)

case_Lazy1_Reader_t1 :: Assertion
case_Lazy1_Reader_t1 = let
  t1' = do v <- LazyR.ask; return (v + 1 :: Int)
  t1r = LazyR.runReader t1 (10::Int)
  in
    -- 'LazyR.run t1' should result in type-error
    11 @=? (run t1r)

t2 = do
  v1 <- LazyR.ask
  v2 <- LazyR.ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))


case_Lazy1_Reader_t2 :: Assertion
case_Lazy1_Reader_t2 = let
  t2r = LazyR.runReader t2 (10::Int)
  t2rr = flip LazyR.runReader (20::Float) . flip LazyR.runReader (10::Int) $ t2
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
  (run $ LazyR.runReader (LazyR.runReader t2 (20::Float)) (10::Int))


case_Lazy1_Reader_t3 :: Assertion
case_Lazy1_Reader_t3 = let
  t3 = t1 `add` LazyR.local (+ (10::Int)) t1
  in
    212 @=? (run $ LazyR.runReader t3 (100::Int))

-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (LazyR.local (+ (10::Int)) t2)
                (LazyR.local (+ (30::Float)) t2)

case_Lazy1_Reader_t4 :: Assertion
case_Lazy1_Reader_t4 = 106.0 @=?
  (run $ LazyR.runReader (LazyR.runReader t4 (20::Float)) (10::Int))

-- The opposite order of layers gives the same result
case_Lazy1_Reader_t4' :: Assertion
case_Lazy1_Reader_t4' = 106.0 @=?
  (run $ LazyR.runReader (LazyR.runReader t4 (20::Float)) (10::Int))

-- Map an effectful function
case_Lazy1_Reader_tmap :: Assertion
case_Lazy1_Reader_tmap = let
  tmap = mapM f [1..5]
  in
    ([11,12,13,14,15] :: [Int]) @=?
    (run $ LazyR.runReader tmap (10::Int))
  where
    f x = LazyR.ask `add` return x

-- {{{ Reader.runReader

case_Lazy1_Reader_runReader :: Assertion
case_Lazy1_Reader_runReader = let
  e = run $ LazyR.runReader voidReader (undefined :: ())
  in
   assertNoUndefined (e :: ())
  where
    voidReader = do
        _ <- (LazyR.ask :: Eff '[LazyR.Reader ()] ())
        return ()

case_Strict1_Reader_runReader :: Assertion
case_Strict1_Reader_runReader = let
  e = run $ StrictR.runReader voidReader (undefined :: ())
  in
   assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (StrictR.ask :: Eff '[StrictR.Reader ()] ())
        return ()

-- }}}

-- }}}

-- {{{ State.runState

-- {{{ Lazy

case_Lazy1_State_runState :: Assertion
case_Lazy1_State_runState = let
  (r, ()) = run
            $ flip LazyS.runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertNoUndefined r
  where
    getVoid :: Eff '[LazyS.State ()] ()
    getVoid = LazyS.get

    putVoid :: () -> Eff '[LazyS.State ()] ()
    putVoid = LazyS.put

-- }}}

-- {{{ Strict1

case_Strict1_State_runState :: Assertion
case_Strict1_State_runState = let
  (r, ()) = run
            $ (flip StrictS.runState) undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertUndefined r
  where
    getVoid :: Eff '[StrictS.State ()] ()
    getVoid = StrictS.get

    putVoid :: () -> Eff '[StrictS.State ()] ()
    putVoid = StrictS.put

case_Strict1_State_ts1 :: Assertion
case_Strict1_State_ts1 = (10,10) @=? (run (StrictS.runState ts1 (0::Int)))
  where
    ts1 = do
      StrictS.put (10 ::Int)
      x <- StrictS.get
      return (x::Int)

case_Strict1_State_ts11 :: Assertion
case_Strict1_State_ts11 =
  (10,10) @=? (run (StrictS.runStateR ts11 (0::Int)))
  where
    ts11 = do
      StrictW.tell (10 ::Int)
      x <- StrictR.ask
      return (x::Int)

case_Strict1_State_ts2 :: Assertion
case_Strict1_State_ts2 = (30::Int,20::Int) @=?
  (run (StrictS.runState ts2 (0::Int)))
  where
    ts2 = do
      StrictS.put (10::Int)
      x <- StrictS.get
      StrictS.put (20::Int)
      y <- StrictS.get
      return (x+y)

case_Strict1_State_ts21 :: Assertion
case_Strict1_State_ts21 = (30::Int,20::Int) @=?
  (run (StrictS.runStateR ts21 (0::Int)))
  where
    ts21 = do
      StrictW.tell (10::Int)
      x <- StrictR.ask
      StrictW.tell (20::Int)
      y <- StrictR.ask
      return (x+y)

tes1 :: (Member (StrictS.State Int) r
        , Member (Exc [Char]) r) => Eff r b
tes1 = do
  incr
  throwExc "exc"
  where
    incr = StrictS.get >>= StrictS.put . (+ (1::Int))

case_Strict1_State_ter1 :: Assertion
case_Strict1_State_ter1 = (Left "exc" :: Either String Int,2) @=?
  (run $ StrictS.runState (runExc tes1) (1::Int))

case_Strict1_State_ter2 :: Assertion
case_Strict1_State_ter2 = (Left "exc" :: Either String (Int,Int)) @=?
  (run $ runExc (StrictS.runState tes1 (1::Int)))

teCatch :: Member (Exc String) r => Eff r a -> Eff r [Char]
teCatch m = catchExc (m >> return "done") (\e -> return (e::String))

case_Strict1_State_ter3 :: Assertion
case_Strict1_State_ter3 = (Right "exc" :: Either String String,2) @=?
  (run $ StrictS.runState (runExc (teCatch tes1)) (1::Int))

case_Strict1_State_ter4 :: Assertion
case_Strict1_State_ter4 = (Right ("exc",2) :: Either String (String,Int)) @=?
  (run $ runExc (StrictS.runState (teCatch tes1) (1::Int)))

-- }}}

-- }}}

-- {{{ Writer

addGet :: Member (LazyR.Reader Int) r  => Int -> Eff r Int
addGet x = LazyR.ask >>= \i -> return (i+x)

addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

case_Lazy1_Writer_rdwr :: Assertion
case_Lazy1_Writer_rdwr = (10, ["begin", "end"]) @=?
  (run . (`LazyR.runReader` (1::Int)) . LazyW.runListWriter $ rdwr)
  where
    rdwr = do
      LazyW.tell "begin"
      r <- addN 10
      LazyW.tell "end"
      return r

-- {{{ Writer.censor

prop_Lazy1_Writer_censor :: [Integer] -> Property
prop_Lazy1_Writer_censor l =
  property
  $ listE (mapM_ (LazyW.tell . inc) l) == listE (LazyW.censor inc $ mapM_ LazyW.tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: Eff '[LazyW.Writer Integer] () -> [Integer]
    listE = snd . run . LazyW.runListWriter

-- }}}

-- {{{ Writer.runFirstWriter

case_Lazy1_Writer_runFirstWriter :: Assertion
case_Lazy1_Writer_runFirstWriter = let
  ((), Just m) = run $ LazyW.runFirstWriter $ mapM_ LazyW.tell [(), undefined]
  in
   assertNoUndefined (m :: ())

-- }}}

-- {{{ Writer.runLastWriter

case_Lazy1_Writer_runLastWriter :: Assertion
case_Lazy1_Writer_runLastWriter = let
  ((), Just m) = run $ LazyW.runLastWriter $ mapM_ LazyW.tell [undefined, ()]
  in
   assertNoUndefined (m :: ())

case_Strict1_Writer_runLastWriter :: Assertion
case_Strict1_Writer_runLastWriter = let
  ((), Just m) = run $ StrictW.runLastWriter $ mapM_ StrictW.tell [undefined, ()]
  in
   assertUndefined (m :: ())

-- }}}

-- }}}

-- {{{ Exception

-- The type is inferred
-- et1 :: Eff r Int
et1 = return 1 `add` return 2

case_Exception1_et1 :: Assertion
case_Exception1_et1 = 3 @=? (run et1)

-- The type is inferred
-- et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwExc (2::Int)

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    Could not deduce (Data.OpenUnion51.FindElem (Exc Int) '[])
      arising from a use of `et2'
-}

case_Exception1_et21 :: Assertion
case_Exception1_et21 = (Left (2::Int)) @=?
  (run et21)
  where
    -- The inferred type shows that ex21 is now pure
    -- et21 :: Eff r (Either Int Int)

    et21 = runExc et2

-- {{{ TooBig example from paper

-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)
-- The type is inferred
ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwExc (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runExc

case_Exception1_ex2r :: Assertion
case_Exception1_ex2r = (Right 5) @=? (run ex2r)
  where
    ex2r = LazyR.runReader (runErrBig (ex2 LazyR.ask)) (5::Int)

case_Exception1_ex2r1 :: Assertion
case_Exception1_ex2r1 = (Left (TooBig 7)) @=? (run ex2r1)
  where
    ex2r1 = LazyR.runReader (runErrBig (ex2 LazyR.ask)) (7::Int)

-- Different order of handlers (layers)
case_Exception1_ex2r2 :: Assertion
case_Exception1_ex2r2 = (Left (TooBig 7)) @=? (run ex2r2)
  where
    ex2r2 = runErrBig (LazyR.runReader (ex2 LazyR.ask) (7::Int))

-- }}}

-- {{{ Alternative

-- Implementing the operator <|> from Alternative:
--  a <|> b does
--   -- tries a, and if succeeds, returns its result
--   -- otherwise, tries b, and if succeeds, returns its result
--   -- otherwise, throws mappend of exceptions of a and b

-- We use MemberU2 in the signature rather than Member to
-- ensure that the computation throws only one type of exceptions.
-- Otherwise, this construction is not very useful.
alttry :: forall e r a. (Monoid e, MemberU2 Exc (Exc e) r) =>
          Eff r a -> Eff r a -> Eff r a
alttry ma mb =
  catchExc ma $ \ea ->
  catchExc mb $ \eb -> throwExc (mappend (ea::e) eb)

case_Exception1_alttry :: Assertion
case_Exception1_alttry =
  [Right 10,Right 10,Right 10,Left "bummer1bummer2"] @=?
  [
  run . runExc $
     (return 1 `add` throwExc "bummer1") `alttry`
     (return 10),
  run . runExc $
     (return 10) `alttry`
     (return 1 `add` throwExc "bummer2"),
  run . runExc $
     (return 10) `alttry` return 20,
  run . runExc $
     (return 1 `add` throwExc "bummer1") `alttry`
     (return 1 `add` throwExc "bummer2")
     ]

-- }}}

-- {{{ Eff Failure

case_Failure1_Effect :: Assertion
case_Failure1_Effect =
  let go :: Eff (Exc () ': StrictW.Writer Int ': '[]) Int
         -> Int
      go = snd . run . StrictW.runWriter (+) 0 . ignoreFail
      ret = go $ do
        StrictW.tell (1 :: Int)
        StrictW.tell (2 :: Int)
        StrictW.tell (3 :: Int)
        () <- die
        StrictW.tell (4 :: Int)
        return 5
   in assertEqual "Fail should stop writing" 6 ret

-- }}}

-- }}}

-- {{{ Choose

case_Choose1_exc11 :: Assertion
case_Choose1_exc11 = [2,3] @=? (run exc11)
  where
    exc11 = Choose.makeChoice exc1
    exc1 = return 1 `add` Choose.choose [1,2]

case_Choose_ex2 :: Assertion
case_Choose_ex2 =
  let ex2_1 = run . makeChoice . runErrBig $ ex2 (Choose.choose [5,7,1])
      ex2_2 = run . runErrBig . makeChoice $ ex2 (Choose.choose [5,7,1])
  in
    assertEqual "Choose: Combining exceptions and non-determinism: ex2_1"
    expected1 ex2_1
    >> assertEqual "Choose: Combining exceptions and non-determinism: ex2_2"
    expected2 ex2_2
  where
    expected1 = [Right 5,Left (TooBig 7),Right 1]
    expected2 = Left (TooBig 7)

case_Choose_exRec :: Assertion
case_Choose_exRec =
  let exRec_1 = run . runErrBig . makeChoice $ exRec (ex2 (Choose.choose [5,7,1]))
      exRec_2 = run . makeChoice . runErrBig $ exRec (ex2 (Choose.choose [5,7,1]))
      exRec_3 = run . runErrBig . makeChoice $ exRec (ex2 (Choose.choose [5,7,11,1]))
  in
    assertEqual "Choose: error recovery: exRec_1" expected1 exRec_1
    >> assertEqual "Choose: error recovery: exRec_2" expected2 exRec_2
    >> assertEqual "Choose: error recovery: exRec_1" expected3 exRec_3
  where
    expected1 = Right [5,7,1]
    expected2 = [Right 5,Right 7,Right 1]
    expected3 = Left (TooBig 11)
    -- Errror recovery part
    -- The code is the same as in transf1.hs. The inferred signatures differ
    -- Was: exRec :: MonadError TooBig m => m Int -> m Int
    -- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
    exRec m = catchExc m handler
      where handler (TooBig n) | n <= 7 = return n
            handler e = throwExc e

-- }}}

-- {{{ NdetEff

case_NdetEff_testCA :: Assertion
case_NdetEff_testCA = [2, 4..10] @=? (run $ makeChoiceA testCA)
  where
    testCA :: (Integral a) => Eff (NdetEff ': r) a
    testCA = do
      i <- msum . fmap return $ [1..10]
      guard (i `mod` 2 == 0)
      return i

case_NdetEff_ifte :: Assertion
case_NdetEff_ifte =
  let primes = ifte_test_run
  in
    assertEqual "NdetEff: test ifte using primes"
    [2,3,5,7,11,13,17,19,23,29] primes
  where
    ifte_test = do
      n <- gen
      ifte (do
               d <- gen
               guard $ d < n && n `mod` d == 0
               -- _ <- trace ("d: " ++ show d) (return ())
           )
        (\_ -> mzero)
        (return n)
        where gen = msum . fmap return $ [2..30]

    ifte_test_run :: [Int]
    ifte_test_run = run . makeChoiceA $ ifte_test


-- called reflect in the LogicT paper
case_NdetEff_reflect :: Assertion
case_NdetEff_reflect =
  let tsplitr10 = run $ StrictW.runListWriter $ makeChoiceA tsplit
      tsplitr11 = run $ StrictW.runListWriter $ makeChoiceA (msplit tsplit >>= unmsplit)
      tsplitr20 = run $ makeChoiceA $ StrictW.runListWriter tsplit
      tsplitr21 = run $ makeChoiceA $ StrictW.runListWriter (msplit tsplit >>= unmsplit)
  in
    assertEqual "tsplitr10" expected1 tsplitr10
    >> assertEqual "tsplitr11" expected1 tsplitr11
    >> assertEqual "tsplitr20" expected2 tsplitr20
    >> assertEqual "tsplitr21" expected21 tsplitr21
  where
    expected1 = ([1, 2],["begin", "end"])
    expected2 = [(1, ["begin"]), (2, ["end"])]
    expected21 = [(1, ["begin"]), (2, ["begin", "end"])]

    unmsplit :: Member NdetEff r => (Maybe (a, Eff r a)) -> Eff r a
    unmsplit Nothing      = mzero
    unmsplit (Just (a,m)) = return a `mplus` m

    tsplit =
      (StrictW.tell "begin" >> return 1) `mplus`
      (StrictW.tell "end"   >> return 2)

-- }}}

-- {{{ test Lift building

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Monad m, MemberU2 Lift (Lift m) r) => Eff r ()
    possiblyAmbiguous = lift $ return ()

-- }}}

-- {{{ Nested Eff

testNestedEff :: Property
testNestedEff = forAll arbitrary (\x -> property (qu x == x))
  where
    qu :: Bool -> Bool
    qu x = run $ StrictR.runReader (readerAp x) readerId

    readerAp :: Bool -> Eff '[StrictR.Reader (Eff '[StrictR.Reader Bool] Bool)] Bool
    readerAp x = do
      f <- StrictR.ask
      return . run $ StrictR.runReader f x

    readerId :: Eff '[StrictR.Reader Bool] Bool
    readerId = do
      x <- StrictR.ask
      return x

-- }}}

-- {{{ Operational Monad

case_Operational_Monad :: Assertion
case_Operational_Monad =
  let comp :: (Member (LazyS.State [String]) r
               , Member (LazyW.Writer String) r)
              => Eff r ()
      comp = Op.runProgram Op.Eg.adventPure Op.Eg.prog
      go = snd . run . LazyW.runMonoidWriter $ LazyS.evalState comp ["foo", "bar"]
  in
   assertEqual
   "Evaluating Operational Monad example"
   (unlines ["getting input...",
             "ok",
             "the input is foo"]) go

-- }}}

-- {{{ Yield

yieldInt :: Member (Yield Int ()) r => Int -> Eff r ()
yieldInt = yield

case_Coroutines_c1 :: Assertion
case_Coroutines_c1 = do
  ((), actual) <- catchOutput c1
  assertEqual
    "Coroutine: Simple coroutines using Eff"
    (unlines ["1", "2", "Done"]) actual
  where
    th1 :: Member (Yield Int ()) r => Eff r ()
    th1 = yieldInt 1 >> yieldInt 2

    c1 = runTrace (loop =<< runC th1)
      where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
            loop (Done)    = trace ("Done")

case_Coroutines_c2 :: Assertion
case_Coroutines_c2 = do
  ((), actual1) <- catchOutput c2
  assertEqual "Coroutine: Add dynamic variables"
    (unlines ["10", "10", "Done"]) actual1
  ((), actual2) <- catchOutput c21
  assertEqual "Coroutine: locally changing the dynamic environment for the suspension"
    (unlines ["10", "11", "Done"]) actual2
  where
    -- The code is essentially the same as that in transf.hs (only added
    -- a type specializtion on yield). The inferred signature is different though.
    -- Before it was
    --    th2 :: MonadReader Int m => CoT Int m ()
    -- Now it is more general:
    th2 :: (Member (Yield Int ()) r, Member (StrictR.Reader Int) r) => Eff r ()
    th2 = StrictR.ask >>= yieldInt >> (StrictR.ask >>= yieldInt)

    -- Code is essentially the same as in transf.hs; no liftIO though
    c2 = runTrace $ StrictR.runReader (loop =<< runC th2) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
            loop Done    = trace "Done"

    -- locally changing the dynamic environment for the suspension
    c21 = runTrace $ StrictR.runReader (loop =<< runC th2) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> StrictR.local (+(1::Int)) (k ()) >>= loop
            loop Done    = trace "Done"

case_Coroutines_c3 :: Assertion
case_Coroutines_c3 = do
  ((), actual1) <- catchOutput c3
  assertEqual "Coroutine: two sorts of local rebinding"
    (unlines ["10", "10", "20", "20", "Done"]) actual1
  ((), actual2) <- catchOutput c31
  let expected2 = (unlines ["10", "11", "21", "21", "Done"])
  assertEqual "Coroutine: locally changing the dynamic environment for the suspension"
    expected2 actual2
  ((), actual3) <- catchOutput c4
  assertEqual "Coroutine: abstracting the client computation"
    expected2 actual3
  where
    local = StrictR.local
    ask = StrictR.ask
    runReader = StrictR.runReader

    th3 :: (Member (Yield Int ()) r, Member (StrictR.Reader Int) r) => Eff r ()
    th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
      where ay = ask >>= yieldInt

    c3 = runTrace $ runReader (loop =<< runC th3) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
            loop Done    = trace "Done"

    -- The desired result: the coroutine shares the dynamic environment with its
    -- parent; however, when the environment is locally rebound, it becomes
    -- private to coroutine.
    c31 = runTrace $ runReader (loop =<< runC th3) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
            loop Done    = trace "Done"

    -- We now make explicit that the client computation, run by th4,
    -- is abstract. We abstract it out of th4
    c4 = runTrace $ runReader (loop =<< runC (th4 client)) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
            loop Done    = trace "Done"

            -- cl, client, ay are monomorphic bindings
            th4 cl = cl >> local (+(10::Int)) cl
            client = ay >> ay
            ay     = ask >>= yieldInt

case_Corountines_c5 :: Assertion
case_Corountines_c5 = do
  ((), actual) <- catchOutput c5
  let expected = unlines ["10"
                         ,"11"
                         ,"12"
                         ,"18"
                         ,"18"
                         ,"18"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"29"
                         ,"Done"
                         ]
  assertEqual "Corountine: Even more dynamic example"
    expected actual
  where
    local = StrictR.local
    ask = StrictR.ask
    runReader = StrictR.runReader

    c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
      where loop (Y x k) = trace (show (x::Int)) >> local (\y->x+1) (k ()) >>= loop
            loop Done    = trace "Done"

            -- cl, client, ay are monomorphic bindings
            client = ay >> ay >> ay
            ay     = ask >>= yieldInt

            -- There is no polymorphic recursion here
            th cl = do
              cl
              v <- ask
              (if v > (20::Int) then id else local (+(5::Int))) cl
              if v > (20::Int) then return () else local (+(10::Int)) (th cl)

case_Coroutines_c7 :: Assertion
case_Coroutines_c7 = do
  ((), actual) <- catchOutput c7
  let expected = unlines ["1010"
                         ,"1021"
                         ,"1032"
                         ,"1048"
                         ,"1064"
                         ,"1080"
                         ,"1101"
                         ,"1122"
                         ,"1143"
                         ,"1169"
                         ,"1195"
                         ,"1221"
                         ,"1252"
                         ,"1283"
                         ,"1314"
                         ,"1345"
                         ,"1376"
                         ,"1407"
                         ,"Done"
                         ]
  assertEqual "Coroutine: And even more dynamic example"
    expected actual
  where
    local = StrictR.local
    ask = StrictR.ask
    runReader = StrictR.runReader

    c7 = runTrace $
          runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
     where loop (Y x k) = trace (show (x::Int)) >>
                          local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
           loop Done    = trace "Done"

           -- cl, client, ay are monomorphic bindings
           client = ay >> ay >> ay
           ay     = ask >>= \x -> ask >>=
                     \y -> yieldInt (x + round (y::Double))

           -- There is no polymorphic recursion here
           th cl = do
             cl
             v <- ask
             (if v > (20::Int) then id else local (+(5::Int))) cl
             if v > (20::Int) then return () else local (+(10::Int)) (th cl)

case_Coroutines_c7' :: Assertion
case_Coroutines_c7' = do
  ((), actual) <- catchOutput c7'
  let expected = unlines ["1010"
                         ,"1021"
                         ,"1032"
                         ,"1048"
                         ,"1048"
                         ,"1048"
                         ,"1069"
                         ,"1090"
                         ,"1111"
                         ,"1137"
                         ,"1137"
                         ,"1137"
                         ,"1168"
                         ,"1199"
                         ,"1230"
                         ,"1261"
                         ,"1292"
                         ,"1323"
                         ,"Done"
                         ]
  assertEqual "Coroutine: And even more dynamic example"
    expected actual
  where
    local = StrictR.local
    ask = StrictR.ask
    runReader = StrictR.runReader

    c7' = runTrace $
          runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
     where loop (Y x k) = trace (show (x::Int)) >>
                          local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
           loop Done    = trace "Done"

           -- cl, client, ay are monomorphic bindings
           client = ay >> ay >> ay
           ay     = ask >>= \x -> ask >>=
                     \y -> yieldInt (x + round (y::Double))

           -- There is no polymorphic recursion here
           th cl = do
             cl
             v <- ask
             (if v > (20::Int) then id else local (+(5::Double))) cl
             if v > (20::Int) then return () else local (+(10::Int)) (th cl)

-- }}}

-- {{{ Lift

case_Lift_tl1r :: Assertion
case_Lift_tl1r = do
  ((), output) <- catchOutput tl1r
  assertEqual "Test tl1r" (showLn input) output
  where
    input = (5::Int)
    -- tl1r :: IO ()
    tl1r = runLift (StrictR.runReader tl1 input)
      where
        tl1 = StrictR.ask >>= \(x::Int) -> lift . print $ x

case_Lift_tMd' :: Assertion
case_Lift_tMd' = do
  actual <- catchOutput tMd'
  let expected = (output, (showLines input))
  assertEqual "Test mapMdebug using Lift" expected actual
  where
    input = [1..5]
    val = (10::Int)
    output = map (+ val) input

    tMd' = runLift $ StrictR.runReader (mapMdebug' f input) val
      where f x = StrictR.ask `add` return x

    -- Re-implemenation of mapMdebug using Lifting
    -- The signature is inferred
    mapMdebug'  :: (Show a, MemberU2 Lift (Lift IO) r) =>
                   (a -> Eff r b) -> [a] -> Eff r [b]
    mapMdebug' f [] = return []
    mapMdebug' f (h:t) = do
      lift $ print h
      h' <- f h
      t' <- mapMdebug' f t
      return (h':t')

-- }}}

-- {{{ Trace

case_Trace_tdup :: Assertion
case_Trace_tdup = do
  ((), actual) <- catchOutput tdup
  assertEqual "Trace: duplicate layers"
    (unlines ["Asked: 20", "Asked: 10"]) actual
  where
    tdup = runTrace $ StrictR.runReader m (10::Int)
     where
     m = do
         StrictR.runReader tr (20::Int)
         tr
     tr = do
          v <- StrictR.ask
          trace $ "Asked: " ++ show (v::Int)

case_Trace_tMd :: Assertion
case_Trace_tMd = do
  actual <- catchOutput tMd
  assertEqual "Trace: higher-order effectful function"
    (map (+ val) input, unlines $ map (("mapMdebug: " ++) . show) input) actual
  where
    val = (10::Int)
    input = [1..5]
    tMd = runTrace $ StrictR.runReader (mapMdebug f input) val
      where
        f x = StrictR.ask `add` return x

        -- Higher-order effectful function
        -- The inferred type shows that the Trace affect is added to the effects
        -- of r
        mapMdebug:: (Show a, Member Trace r) =>
                    (a -> Eff r b) -> [a] -> Eff r [b]
        mapMdebug f [] = return []
        mapMdebug f (h:t) = do
          trace $ "mapMdebug: " ++ show h
          h' <- f h
          t' <- mapMdebug f t
          return (h':t')

-- }}}

-- {{{ Fresh

case_Fresh_tfresh' :: Assertion
case_Fresh_tfresh' = do
  ((), actual) <- catchOutput tfresh'
  assertEqual "Fresh: test"
    (unlines ["Fresh 0", "Fresh 1"]) actual
  where
    tfresh' = runTrace $ flip runFresh' 0 $ do
      n <- fresh
      trace $ "Fresh " ++ show n
      n <- fresh
      trace $ "Fresh " ++ show n

-- }}}
