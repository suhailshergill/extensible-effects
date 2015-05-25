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

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Operational as Op
import Control.Eff.Operational.Example as Op.Eg
import Control.Eff.Reader.Lazy as LazyR
import Control.Eff.State.Lazy as LazyS
import Control.Eff.Writer.Lazy as LazyW
import Control.Eff.Reader.Strict as StrictR
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
  (total1, ()) = run $ LazyS.runState 0 $ sumAll l
  (last1, ()) = run $ LazyW.runLastWriter $ writeAll l
  (total2, (last2, ())) = run $ LazyS.runState 0 $ LazyW.runLastWriter $ writeAndAdd l
  (last3, (total3, ())) = run $ LazyW.runLastWriter $ LazyS.runState 0 $ writeAndAdd l
  in
   allEqual [safeLast l, last1, last2, last3]
   .&&. allEqual [sum l, total1, total2, total3]
  where
    writeAll :: (Typeable a, Member (LazyW.Writer a) e)
             => [a]
             -> Eff e ()
    writeAll = mapM_ LazyW.tell

    sumAll :: (Typeable a, Num a, Member (LazyS.State a) e)
           => [a]
           -> Eff e ()
    sumAll = mapM_ (LazyS.modify . (+))

    writeAndAdd :: (Member (LazyW.Writer Integer) e, Member (LazyS.State Integer) e)
                => [Integer]
                -> Eff e ()
    writeAndAdd lst = do
        writeAll lst
        sumAll lst

-- }}}

-- {{{ Reader.runReader

case_Lazy_Reader_runReader :: Assertion
case_Lazy_Reader_runReader = let
  e = run $ LazyR.runReader voidReader (undefined :: ())
  in
   assertNoUndefined (e :: ())
  where
    voidReader = do
        _ <- (LazyR.ask :: Eff (LazyR.Reader () :> Void) ())
        return ()

case_Strict_Reader_runReader :: Assertion
case_Strict_Reader_runReader = let
  e = run $ StrictR.runReader voidReader (undefined :: ())
  in
   assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (StrictR.ask :: Eff (StrictR.Reader () :> Void) ())
        return ()

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

-- {{{ Writer.censor

prop_Lazy_Writer_censor :: [Integer] -> Property
prop_Lazy_Writer_censor l =
  property
  $ listE (mapM_ (LazyW.tell . inc) l) == listE (LazyW.censor inc $ mapM_ LazyW.tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: Eff (LazyW.Writer Integer :> Void) () -> [Integer]
    listE = fst . run . LazyW.runWriter (:) []

-- }}}

-- {{{ Writer.runFirstWriter

case_Lazy_Writer_runFirstWriter :: Assertion
case_Lazy_Writer_runFirstWriter = let
  (Just m, ()) = run $ LazyW.runFirstWriter $ mapM_ LazyW.tell [(), undefined]
  in
   assertNoUndefined (m :: ())

-- }}}

-- {{{ Writer.runLastWriter

case_Lazy_Writer_runLastWriter :: Assertion
case_Lazy_Writer_runLastWriter = let
  (Just m, ()) = run $ LazyW.runLastWriter $ mapM_ LazyW.tell [undefined, ()]
  in
   assertNoUndefined (m :: ())

case_Strict_Writer_runLastWriter :: Assertion
case_Strict_Writer_runLastWriter = let
  (Just m, ()) = run $ StrictW.runLastWriter $ mapM_ StrictW.tell [undefined, ()]
  in
   assertUndefined (m :: ())

-- }}}

-- }}}

-- {{{ Eff Failure

case_Failure_Effect :: Assertion
case_Failure_Effect =
  let go :: Eff (Exc () :> StrictW.Writer Int :> Void) Int
         -> Int
      go = fst . run . StrictW.runWriter (+) 0 . ignoreFail
      ret = go $ do
        StrictW.tell (1 :: Int)
        StrictW.tell (2 :: Int)
        StrictW.tell (3 :: Int)
        () <- die
        StrictW.tell (4 :: Int)
        return 5
   in assertEqual "Fail should stop writing" 6 ret

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
    qu x = run $ StrictR.runReader (readerAp x) readerId

    readerAp :: Bool -> Eff (StrictR.Reader (Eff (StrictR.Reader Bool :> Void) Bool) :> Void) Bool
    readerAp x = do
      f <- StrictR.ask
      return . run $ StrictR.runReader f x

    readerId :: Eff (StrictR.Reader Bool :> Void) Bool
    readerId = do
      x <- StrictR.ask
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
