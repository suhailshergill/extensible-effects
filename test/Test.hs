{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Exception (Exception, ErrorCall, catch)
import Control.Monad (void)
import Data.Typeable

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit hiding (State)
import Test.QuickCheck

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy as LazyR
import Control.Eff.State.Lazy as LazyS
import Control.Eff.Writer.Lazy as LazyW
import Control.Eff.Reader.Strict as StrictR
import Control.Eff.State.Strict as StrictS
import Control.Eff.Writer.Strict as StrictW

withError :: a -> ErrorCall -> a
withError a _ = a

assertUndefined :: a -> Assertion
assertUndefined a = catch (seq a $ assertFailure "") (withError $ return ())

assertNoUndefined :: a -> Assertion
assertNoUndefined a = catch (seq a $ return ()) (withError $ assertFailure "")

main :: IO ()
main = defaultMain tests

allEqual :: Eq a => [a] -> Bool
allEqual = all (uncurry (==)) . pairs
  where
    pairs l = zip l $ tail l

safeLast [] = Nothing
safeLast l = Just $ last l

testDocs :: [Integer] -> Property
testDocs l = let
              (total1, ()) = run $ LazyS.runState 0 $ sumAll l
              (last1, ()) = run $ LazyW.runLastWriter $ writeAll l
              (total2, (last2, ())) = run $ LazyS.runState 0 $ LazyW.runLastWriter $ writeAndAdd l
              (last3, (total3, ())) = run $ LazyW.runLastWriter $ LazyS.runState 0 $ writeAndAdd l
             in allEqual [safeLast l, last1, last2, last3]
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
    writeAndAdd l = do
        writeAll l
        sumAll l

testCensor :: [Integer] -> Property
testCensor l = property
             $ listE (mapM_ (LazyW.tell . inc) l) == listE (LazyW.censor inc $ mapM_ LazyW.tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: Eff (LazyW.Writer Integer :> ()) () -> [Integer]
    listE = fst . run . LazyW.runWriter (:) []

testReaderLaziness :: Assertion
testReaderLaziness = let e = run $ LazyR.runReader voidReader (undefined :: ())
                     in assertNoUndefined (e :: ())
  where
    voidReader = do
        _ <- (LazyR.ask :: Eff (LazyR.Reader () :> ()) ())
        return ()

testReaderStrictness :: Assertion
testReaderStrictness = let e = run $ StrictR.runReader voidReader (undefined :: ())
                       in assertUndefined (e :: ())
  where
    voidReader = do
        _ <- (StrictR.ask :: Eff (StrictR.Reader () :> ()) ())
        return ()

testStateLaziness :: Assertion
testStateLaziness = let (r, ()) = run
                                $ LazyS.runState undefined
                                $ getVoid
                               >> putVoid undefined
                               >> putVoid ()
                    in assertNoUndefined r
  where
    getVoid :: Eff (LazyS.State () :> ()) ()
    getVoid = LazyS.get

    putVoid :: () -> Eff (LazyS.State () :> ()) ()
    putVoid = LazyS.put

testStateStrictness :: Assertion
testStateStrictness = let (r, ()) = run
                                  $ StrictS.runState undefined
                                  $ getVoid
                                 >> putVoid undefined
                                 >> putVoid ()
                      in assertUndefined r
  where
    getVoid :: Eff (StrictS.State () :> ()) ()
    getVoid = StrictS.get

    putVoid :: () -> Eff (StrictS.State () :> ()) ()
    putVoid = StrictS.put

testLastWriterLaziness :: Assertion
testLastWriterLaziness = let (Just m, ()) = run $ LazyW.runLastWriter $ mapM_ LazyW.tell [undefined, ()]
                         in assertNoUndefined (m :: ())

testLastWriterStrictness :: Assertion
testLastWriterStrictness = let (Just m, ()) = run $ StrictW.runLastWriter $ mapM_ StrictW.tell [undefined, ()]
                           in assertUndefined (m :: ())

testFirstWriterLaziness :: Assertion
testFirstWriterLaziness = let (Just m, ()) = run $ LazyW.runFirstWriter $ mapM_ LazyW.tell [(), undefined]
                          in assertNoUndefined (m :: ())

tests =
  [ testProperty "Documentation example." testDocs
  , testCase "Test runReader laziness." testReaderLaziness
  , testCase "Test runReader strictness." testReaderStrictness
  , testCase "Test runState laziness." testStateLaziness
  , testCase "Test runState strictness." testStateStrictness
  , testCase "Test runLastWriter laziness." testLastWriterLaziness
  , testCase "Test runLastWriter strictness." testLastWriterStrictness
  , testCase "Test runFirstWriter laziness." testFirstWriterLaziness
  ]
