{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Control.Exception (ErrorCall, catch)
import Data.Typeable

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import Test.HUnit hiding (State)
import Test.QuickCheck

import qualified Control.Eff1 as E1
import qualified Data.OpenUnion51 as OU51
import qualified Control.Eff.Reader.Lazy1 as E1.LazyR
import qualified Control.Eff.Reader.Strict1 as E1.StrictR
import qualified Control.Eff.Writer.Lazy1 as E1.LazyW
import qualified Control.Eff.Writer.Strict1 as E1.StrictW
import Control.Eff.Exception1
import Control.Monad (liftM2)
import Control.Eff.Choose1 as E1.Choose
import Data.Monoid

import Control.Eff
import Control.Eff.Example as Eg
import Control.Eff.Lift
import Control.Eff.Operational as Op
import Control.Eff.Operational.Example as Op.Eg
import Control.Eff.State.Lazy as LazyS
import Control.Eff.Writer.Lazy as LazyW
import Control.Eff.State.Strict as StrictS
import Control.Eff.Writer.Strict as StrictW
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

prop_Documentation_example :: [Integer] -> Property
prop_Documentation_example l = let
  (total1, ()) = run $ LazyS.runState 0 $ Eg.sumAll l
  (last1, ()) = run $ LazyW.runLastWriter $ Eg.writeAll l
  (total2, (last2, ())) = run $ LazyS.runState 0 $ LazyW.runLastWriter $ Eg.writeAndAdd l
  (last3, (total3, ())) = run $ LazyW.runLastWriter $ LazyS.runState 0 $ Eg.writeAndAdd l
  in
   allEqual [safeLast l, last1, last2, last3]
   .&&. allEqual [sum l, total1, total2, total3]

-- }}}

-- {{{ Reader

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

case_Lazy_State_runState :: Assertion
case_Lazy_State_runState = let
  (r, ()) = run
            $ LazyS.runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertNoUndefined r
  where
    getVoid :: Eff (LazyS.State () :> Void) ()
    getVoid = LazyS.get

    putVoid :: () -> Eff (LazyS.State () :> Void) ()
    putVoid = LazyS.put

case_Strict_State_runState :: Assertion
case_Strict_State_runState = let
  (r, ()) = run
            $ StrictS.runState undefined
            $ getVoid
            >> putVoid undefined
            >> putVoid ()
  in
   assertUndefined r
  where
    getVoid :: Eff (StrictS.State () :> Void) ()
    getVoid = StrictS.get

    putVoid :: () -> Eff (StrictS.State () :> Void) ()
    putVoid = StrictS.put

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
    exc11 = E1.Choose.runChoice exc1
    exc1 = return 1 `add` E1.Choose.choose [1,2]

-- }}}

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif

-- {{{ test Lift building

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Typeable1 m, Monad m, SetMember Lift (Lift m) r) => Eff r ()
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
  let comp :: (Member (LazyS.State [String]) r
               , Member (LazyW.Writer String) r)
              => Eff r ()
      comp = Op.runProgram Op.Eg.adventPure Op.Eg.prog
      go = fst . run . LazyW.runMonoidWriter . LazyS.evalState ["foo", "bar"] $ comp
  in
   assertEqual
   "Evaluating Operational Monad example"
   "getting input...\nok\nthe input is foo\n" go

-- }}}
