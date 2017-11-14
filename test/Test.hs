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

import qualified Control.Eff as E1
import qualified Data.OpenUnion51 as OU51
import qualified Control.Eff.Reader.Lazy as E1.LazyR
import qualified Control.Eff.Reader.Strict as E1.StrictR
import qualified Control.Eff.Writer.Lazy as E1.LazyW
import qualified Control.Eff.Writer.Strict as E1.StrictW
import qualified Control.Eff.State.Strict as E1.StrictS
import qualified Control.Eff.State.Lazy as E1.LazyS
import Control.Eff.Exception
import Control.Eff.Choose as E1.Choose
import Control.Eff.NdetEff
import Control.Monad (liftM2, msum, guard)
import Data.Monoid

import Control.Eff.Example as E1.Eg
--import Control.Eff.Cut
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.Operational as Op
import Control.Eff.Operational.Example as Op.Eg
import Control.Eff.Trace
import Control.Eff.Coroutine
import Data.Void

main :: IO ()
main = defaultMain tests

tests = [
  $(testGroupGenerator)
#if __GLASGOW_HASKELL__ >= 708
  , testProperty "Test nested Eff." testNestedEff
#endif
        ]

-- {{{ utils

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

-- {{{ Documentation example

-- prop_Documentation_example :: [Integer] -> Property
-- prop_Documentation_example l = let
--   (total1, ()) = run $ LazyS.runState 0 $ Eg.sumAll l
--   (last1, ()) = run $ LazyW.runLastWriter $ Eg.writeAll l
--   (total2, (last2, ())) = run $ LazyS.runState 0 $ LazyW.runLastWriter $ Eg.writeAndAdd l
--   (last3, (total3, ())) = run $ LazyW.runLastWriter $ LazyS.runState 0 $ Eg.writeAndAdd l
--   in
--    allEqual [safeLast l, last1, last2, last3]
--    .&&. allEqual [sum l, total1, total2, total3]

-- }}}

-- {{{ Reader

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)
t1 = E1.LazyR.ask `add` return (1::Int)

case_Lazy1_Reader_t1 :: Assertion
case_Lazy1_Reader_t1 = let
  t1' = do v <- E1.LazyR.ask; return (v + 1 :: Int)
  t1r = E1.LazyR.runReader t1 (10::Int)
  in
    -- 'E1.LazyR.run t1' should result in type-error
    11 @=? (E1.run t1r)

t2 = do
  v1 <- E1.LazyR.ask
  v2 <- E1.LazyR.ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))


case_Lazy1_Reader_t2 :: Assertion
case_Lazy1_Reader_t2 = let
  t2r = E1.LazyR.runReader t2 (10::Int)
  t2rr = flip E1.LazyR.runReader (20::Float) . flip E1.LazyR.runReader (10::Int) $ t2
  in
    33.0 @=? (E1.run t2rr)

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}
case_Lazy1_Reader_t2' :: Assertion
case_Lazy1_Reader_t2' = 33.0 @=?
  (E1.run $ E1.LazyR.runReader (E1.LazyR.runReader t2 (20::Float)) (10::Int))


case_Lazy1_Reader_t3 :: Assertion
case_Lazy1_Reader_t3 = let
  t3 = t1 `add` E1.LazyR.local (+ (10::Int)) t1
  in
    212 @=? (E1.run $ E1.LazyR.runReader t3 (100::Int))

-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (E1.LazyR.local (+ (10::Int)) t2)
                (E1.LazyR.local (+ (30::Float)) t2)

case_Lazy1_Reader_t4 :: Assertion
case_Lazy1_Reader_t4 = 106.0 @=?
  (E1.run $ E1.LazyR.runReader (E1.LazyR.runReader t4 (20::Float)) (10::Int))

-- The opposite order of layers gives the same result
case_Lazy1_Reader_t4' :: Assertion
case_Lazy1_Reader_t4' = 106.0 @=?
  (E1.run $ E1.LazyR.runReader (E1.LazyR.runReader t4 (20::Float)) (10::Int))

-- Map an effectful function
case_Lazy1_Reader_tmap :: Assertion
case_Lazy1_Reader_tmap = let
  tmap = mapM f [1..5]
  in
    ([11,12,13,14,15] :: [Int]) @=?
    (E1.run $ E1.LazyR.runReader tmap (10::Int))
  where
    f x = E1.LazyR.ask `add` return x

-- {{{ Reader.runReader

case_Lazy1_Reader_runReader :: Assertion
case_Lazy1_Reader_runReader = let
  e = E1.run $ E1.LazyR.runReader voidReader (undefined :: ())
  in
   assertNoUndefined (e :: ())
  where
    voidReader = do
        _ <- (E1.LazyR.ask :: E1.Eff '[E1.LazyR.Reader ()] ())
        return ()

case_Strict1_Reader_runReader :: Assertion
case_Strict1_Reader_runReader = let
  e = E1.run $ E1.StrictR.runReader voidReader (undefined :: ())
  in
   assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (E1.StrictR.ask :: E1.Eff '[E1.StrictR.Reader ()] ())
        return ()

-- }}}

-- }}}

-- {{{ State.runState

-- {{{ Lazy

case_Lazy1_State_runState :: Assertion
case_Lazy1_State_runState = let
  (r, ()) = E1.run
            $ flip E1.LazyS.runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertNoUndefined r
  where
    getVoid :: E1.Eff '[E1.LazyS.State ()] ()
    getVoid = E1.LazyS.get

    putVoid :: () -> E1.Eff '[E1.LazyS.State ()] ()
    putVoid = E1.LazyS.put

-- }}}

-- {{{ Strict1

case_Strict1_State_runState :: Assertion
case_Strict1_State_runState = let
  (r, ()) = E1.run
            $ (flip E1.StrictS.runState) undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertUndefined r
  where
    getVoid :: E1.Eff '[E1.StrictS.State ()] ()
    getVoid = E1.StrictS.get

    putVoid :: () -> E1.Eff '[E1.StrictS.State ()] ()
    putVoid = E1.StrictS.put

case_Strict1_State_ts1 :: Assertion
case_Strict1_State_ts1 = (10,10) @=? (E1.run (E1.StrictS.runState ts1 (0::Int)))
  where
    ts1 = do
      E1.StrictS.put (10 ::Int)
      x <- E1.StrictS.get
      return (x::Int)

case_Strict1_State_ts11 :: Assertion
case_Strict1_State_ts11 =
  (10,10) @=? (E1.run (E1.StrictS.runStateR ts11 (0::Int)))
  where
    ts11 = do
      E1.StrictW.tell (10 ::Int)
      x <- E1.StrictR.ask
      return (x::Int)

case_Strict1_State_ts2 :: Assertion
case_Strict1_State_ts2 = (30::Int,20::Int) @=?
  (E1.run (E1.StrictS.runState ts2 (0::Int)))
  where
    ts2 = do
      E1.StrictS.put (10::Int)
      x <- E1.StrictS.get
      E1.StrictS.put (20::Int)
      y <- E1.StrictS.get
      return (x+y)

case_Strict1_State_ts21 :: Assertion
case_Strict1_State_ts21 = (30::Int,20::Int) @=?
  (E1.run (E1.StrictS.runStateR ts21 (0::Int)))
  where
    ts21 = do
      E1.StrictW.tell (10::Int)
      x <- E1.StrictR.ask
      E1.StrictW.tell (20::Int)
      y <- E1.StrictR.ask
      return (x+y)

tes1 :: (OU51.Member (E1.StrictS.State Int) r
        , OU51.Member (Exc [Char]) r) => E1.Eff r b
tes1 = do
  incr
  throwExc "exc"
  where
    incr = E1.StrictS.get >>= E1.StrictS.put . (+ (1::Int))

case_Strict1_State_ter1 :: Assertion
case_Strict1_State_ter1 = (Left "exc" :: Either String Int,2) @=?
  (E1.run $ E1.StrictS.runState (runExc tes1) (1::Int))

case_Strict1_State_ter2 :: Assertion
case_Strict1_State_ter2 = (Left "exc" :: Either String (Int,Int)) @=?
  (E1.run $ runExc (E1.StrictS.runState tes1 (1::Int)))

teCatch :: OU51.Member (Exc String) r => E1.Eff r a -> E1.Eff r [Char]
teCatch m = catchExc (m >> return "done") (\e -> return (e::String))

case_Strict1_State_ter3 :: Assertion
case_Strict1_State_ter3 = (Right "exc" :: Either String String,2) @=?
  (E1.run $ E1.StrictS.runState (runExc (teCatch tes1)) (1::Int))

case_Strict1_State_ter4 :: Assertion
case_Strict1_State_ter4 = (Right ("exc",2) :: Either String (String,Int)) @=?
  (E1.run $ runExc (E1.StrictS.runState (teCatch tes1) (1::Int)))

-- }}}

-- }}}

-- {{{ Writer

addGet :: OU51.Member (E1.LazyR.Reader Int) r  => Int -> E1.Eff r Int
addGet x = E1.LazyR.ask >>= \i -> return (i+x)

addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

case_Lazy1_Writer_rdwr :: Assertion
case_Lazy1_Writer_rdwr = (10, ["begin", "end"]) @=?
  (E1.run . (`E1.LazyR.runReader` (1::Int)) . E1.LazyW.runListWriter $ rdwr)
  where
    rdwr = do
      E1.LazyW.tell "begin"
      r <- addN 10
      E1.LazyW.tell "end"
      return r

-- {{{ Writer.censor

prop_Lazy1_Writer_censor :: [Integer] -> Property
prop_Lazy1_Writer_censor l =
  property
  $ listE (mapM_ (E1.LazyW.tell . inc) l) == listE (E1.LazyW.censor inc $ mapM_ E1.LazyW.tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: E1.Eff '[E1.LazyW.Writer Integer] () -> [Integer]
    listE = snd . E1.run . E1.LazyW.runListWriter

-- }}}

-- {{{ Writer.runFirstWriter

case_Lazy1_Writer_runFirstWriter :: Assertion
case_Lazy1_Writer_runFirstWriter = let
  ((), Just m) = E1.run $ E1.LazyW.runFirstWriter $ mapM_ E1.LazyW.tell [(), undefined]
  in
   assertNoUndefined (m :: ())

-- }}}

-- {{{ Writer.runLastWriter

case_Lazy1_Writer_runLastWriter :: Assertion
case_Lazy1_Writer_runLastWriter = let
  ((), Just m) = E1.run $ E1.LazyW.runLastWriter $ mapM_ E1.LazyW.tell [undefined, ()]
  in
   assertNoUndefined (m :: ())

case_Strict1_Writer_runLastWriter :: Assertion
case_Strict1_Writer_runLastWriter = let
  ((), Just m) = E1.run $ E1.StrictW.runLastWriter $ mapM_ E1.StrictW.tell [undefined, ()]
  in
   assertUndefined (m :: ())

-- }}}

-- }}}

-- {{{ Exception

-- The type is inferred
-- et1 :: Eff r Int
et1 = return 1 `add` return 2

case_Exception1_et1 :: Assertion
case_Exception1_et1 = 3 @=? (E1.run et1)

-- The type is inferred
-- et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwExc (2::Int)

-- The following won't type: unhandled exception!
-- ex2rw = E1.run et2
{-
    Could not deduce (Data.OpenUnion51.FindElem (Exc Int) '[])
      arising from a use of `et2'
-}

case_Exception1_et21 :: Assertion
case_Exception1_et21 = (Left (2::Int)) @=?
  (E1.run et21)
  where
    -- The inferred type shows that ex21 is now pure
    -- et21 :: E1.Eff r (Either Int Int)

    et21 = runExc et2

-- {{{ TooBig example from paper

-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)
-- The type is inferred
ex2 :: OU51.Member (Exc TooBig) r => E1.Eff r Int -> E1.Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwExc (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: E1.Eff (Exc TooBig ': r) a -> E1.Eff r (Either TooBig a)
runErrBig = runExc

case_Exception1_ex2r :: Assertion
case_Exception1_ex2r = (Right 5) @=? (E1.run ex2r)
  where
    ex2r = E1.LazyR.runReader (runErrBig (ex2 E1.LazyR.ask)) (5::Int)

case_Exception1_ex2r1 :: Assertion
case_Exception1_ex2r1 = (Left (TooBig 7)) @=? (E1.run ex2r1)
  where
    ex2r1 = E1.LazyR.runReader (runErrBig (ex2 E1.LazyR.ask)) (7::Int)

-- Different order of handlers (layers)
case_Exception1_ex2r2 :: Assertion
case_Exception1_ex2r2 = (Left (TooBig 7)) @=? (E1.run ex2r2)
  where
    ex2r2 = runErrBig (E1.LazyR.runReader (ex2 E1.LazyR.ask) (7::Int))

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
alttry :: forall e r a. (Monoid e, OU51.MemberU2 Exc (Exc e) r) =>
          E1.Eff r a -> E1.Eff r a -> E1.Eff r a
alttry ma mb =
  catchExc ma $ \ea ->
  catchExc mb $ \eb -> throwExc (mappend (ea::e) eb)

case_Exception1_alttry :: Assertion
case_Exception1_alttry =
  [Right 10,Right 10,Right 10,Left "bummer1bummer2"] @=?
  [
  E1.run . runExc $
     (return 1 `add` throwExc "bummer1") `alttry`
     (return 10),
  E1.run . runExc $
     (return 10) `alttry`
     (return 1 `add` throwExc "bummer2"),
  E1.run . runExc $
     (return 10) `alttry` return 20,
  E1.run . runExc $
     (return 1 `add` throwExc "bummer1") `alttry`
     (return 1 `add` throwExc "bummer2")
     ]

-- }}}

-- {{{ Eff Failure

case_Failure1_Effect :: Assertion
case_Failure1_Effect =
  let go :: E1.Eff (Exc () ': E1.StrictW.Writer Int ': '[]) Int
         -> Int
      go = snd . E1.run . E1.StrictW.runWriter (+) 0 . ignoreFail
      ret = go $ do
        E1.StrictW.tell (1 :: Int)
        E1.StrictW.tell (2 :: Int)
        E1.StrictW.tell (3 :: Int)
        () <- die
        E1.StrictW.tell (4 :: Int)
        return 5
   in assertEqual "Fail should stop writing" 6 ret

-- }}}

-- }}}

-- {{{ Choose

case_Choose1_exc11 :: Assertion
case_Choose1_exc11 = [2,3] @=? (E1.run exc11)
  where
    exc11 = E1.Choose.makeChoice exc1
    exc1 = return 1 `add` E1.Choose.choose [1,2]

-- }}}

-- {{{ NdetEff

case_NdetEff_testCA :: Assertion
case_NdetEff_testCA = [2, 4..10] @=? (E1.run $ makeChoiceA testCA)
  where
    testCA :: (Integral a) => E1.Eff (NdetEff ': r) a
    testCA = do
      i <- msum . fmap return $ [1..10]
      guard (i `mod` 2 == 0)
      return i

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif

-- }}}

-- {{{ test Lift building

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Monad m, OU51.MemberU2 Lift (Lift m) r) => E1.Eff r ()
    possiblyAmbiguous = lift $ return ()

-- }}}

-- {{{ Nested Eff

#if __GLASGOW_HASKELL__ >= 708
testNestedEff :: Property
testNestedEff = forAll arbitrary (\x -> property (qu x == x))
  where
    qu :: Bool -> Bool
    qu x = E1.run $ E1.StrictR.runReader (readerAp x) readerId

    readerAp :: Bool -> E1.Eff '[E1.StrictR.Reader (E1.Eff '[E1.StrictR.Reader Bool] Bool)] Bool
    readerAp x = do
      f <- E1.StrictR.ask
      return . E1.run $ E1.StrictR.runReader f x

    readerId :: E1.Eff '[E1.StrictR.Reader Bool] Bool
    readerId = do
      x <- E1.StrictR.ask
      return x
#endif

-- }}}

-- {{{ Operational Monad

case_Operational_Monad :: Assertion
case_Operational_Monad =
  let comp :: (OU51.Member (E1.LazyS.State [String]) r
               , OU51.Member (E1.LazyW.Writer String) r)
              => E1.Eff r ()
      comp = Op.runProgram Op.Eg.adventPure Op.Eg.prog
      go = snd . E1.run . E1.LazyW.runMonoidWriter $ E1.LazyS.evalState comp ["foo", "bar"]
  in
   assertEqual
   "Evaluating Operational Monad example"
   "getting input...\nok\nthe input is foo\n" go

-- }}}

-- {{{ Yield

-- case_Coroutines_c1 :: Assertion
-- case_Coroutines_c1 =
--   let th1 :: Member (Yield Int ()) r => E1.Eff r ()
--       th1 = yieldInt 1 >> yieldInt 2
--       c1 = runTrace (loop =<< runC th1)
--         where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
--               loop (Done)    = trace ("Done")
--   in
--     assertEqual
--     "Simple coroutines using Eff"
--     "" c1

-- }}}

-- {{{ TODO: Lift

tl1 = E1.StrictR.ask >>= \(x::Int) -> lift . print $ x

-- tl1r :: IO ()
tl1r = runLift (E1.StrictR.runReader tl1 (5::Int))
-- 5

-- Re-implemenation of mapMdebug using Lifting
-- The signature is inferred
mapMdebug'  :: (Show a, OU51.MemberU2 Lift (Lift IO) r) =>
             (a -> E1.Eff r b) -> [a] -> E1.Eff r [b]
mapMdebug' f [] = return []
mapMdebug' f (h:t) = do
 lift $ print h
 h' <- f h
 t' <- mapMdebug' f t
 return (h':t')

tMd' = runLift $ E1.StrictR.runReader (mapMdebug' f [1..5]) (10::Int)
 where f x = E1.StrictR.ask `add` return x
{-
1
2
3
4
5
[11,12,13,14,15]
-}

-- }}}

-- {{{ TODO: Trace

-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, OU51.Member Trace r) =>
     (a -> E1.Eff r b) -> [a] -> E1.Eff r [b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
 trace $ "mapMdebug: " ++ show h
 h' <- f h
 t' <- mapMdebug f t
 return (h':t')

tMd = runTrace $ E1.StrictR.runReader (mapMdebug f [1..5]) (10::Int)
 where f x = E1.StrictR.ask `add` return x
{-
mapMdebug: 1
mapMdebug: 2
mapMdebug: 3
mapMdebug: 4
mapMdebug: 5
[11,12,13,14,15]
-}

-- duplicate layers
tdup = runTrace $ E1.StrictR.runReader m (10::Int)
 where
 m = do
     E1.StrictR.runReader tr (20::Int)
     tr
 tr = do
      v <- E1.StrictR.ask
      trace $ "Asked: " ++ show (v::Int)
{-
Asked: 20
Asked: 10
-}

-- }}}

-- {{{ TODO: Fresh


-- Test
tfresh' = runTrace $ flip runFresh' 0 $ do
  n <- fresh
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n
{-
Fresh 0
Fresh 1
-}

-- }}}

-- {{{ TODO: choice, non-determinism

-- -- primes (very inefficiently -- but a good example of ifte)
-- test_ifte = do
--   n <- gen
--   ifte (do
--      d <- gen
--      guard $ d < n && n `mod` d == 0
--      -- _ <- trace ("d: " ++ show d) (return ())
--     )
--     (\_->mzero)
--     (return n)
--  where gen = msum . fmap return $ [2..30]

-- test_ifte_run :: [Int]
-- test_ifte_run = run . makeChoiceA $ test_ifte
-- -- [2,3,5,7,11,13,17,19,23,29]

-- -- called reflect in the LogicT paper
-- unmsplit :: Member NdetEff r => (Maybe (a, Eff r a)) -> Eff r a
-- unmsplit Nothing      = mzero
-- unmsplit (Just (a,m)) = return a `mplus` m

-- tsplit =
--   (tell "begin" >> return 1) `mplus`
--   (tell "end"   >> return 2)

-- tsplitr10, tsplitr11 :: ([Int],[String])
-- tsplitr10 = run $ runWriter $ makeChoiceA tsplit
-- tsplitr11 = run $ runWriter $ makeChoiceA (msplit tsplit >>= unmsplit)


-- tsplitr20, tsplitr21 :: [(Int,[String])]
-- tsplitr20 = run $ makeChoiceA $ runWriter tsplit
-- tsplitr21 = run $ makeChoiceA $ runWriter (msplit tsplit >>= unmsplit)

-- -- ------------------------------------------------------------------------
-- -- Combining exceptions and non-determinism

-- -- Example from the paper

-- ex2_2 = ([Right 5,Left (TooBig 7),Right 1] ==) $
--         run . makeChoice . runErrBig $ ex2 (choose [5,7,1])

-- -- just like ex1_1 in transf.hs but not at all like ex2_1 in transf.hs

-- -- with different order of handlers, obtain the desired result of
-- -- a high-priority exception
-- ex2_1 = (Left (TooBig 7) ==) $
--         run . runErrBig . makeChoice $ ex2 (choose [5,7,1])


-- -- Errror recovery part
-- -- The code is the same as in transf1.hs. The inferred signatures differ
-- -- Was: exRec :: MonadError TooBig m => m Int -> m Int
-- -- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
-- exRec m = catchError m handler
--  where handler (TooBig n) | n <= 7 = return n
--        handler e = throwError e

-- ex2r_2 = (Right [5,7,1] ==) $
--          run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,1]))
-- -- Compare with ex2r_1 from transf1.hs

-- ex2r_2' = ([Right 5,Right 7,Right 1] ==) $
--           run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,1]))
-- -- Again, all three choices are accounted for.

-- ex2r_1 = (Left (TooBig 11) ==) $
--          run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,11,1]))
-- -- Compare with ex2r_2 from transf1.hs

-- }}}

-- {{{ TODO: cut

-- -- The signature is inferred
-- tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
-- tcut1 = (return (1::Int) `mplus'` return 2) `mplus'`
--          ((cutfalse `mplus'` return 4) `mplus'`
--           return 5)

-- tcut1r = run . makeChoice $ call tcut1
-- -- [1,2]

-- tcut2 = return (1::Int) `mplus'`
--          call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'`
--                return 4)
--        `mplus'` return 5

-- -- Here we see nested call. It poses no problems...
-- tcut2r = run . makeChoice $ call tcut2
-- -- [1,2,5]

-- -- More nested calls
-- tcut3 = call tcut1 `mplus'` call (tcut2 `mplus'` cutfalse)
-- tcut3r = run . makeChoice $ call tcut3
-- -- [1,2,1,2,5]

-- tcut4 = call tcut1 `mplus'`  (tcut2 `mplus'` cutfalse)
-- tcut4r = run . makeChoice $ call tcut4
-- -- [1,2,1,2,5]

-- }}}
