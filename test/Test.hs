{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Writer.Lazy

import Control.Monad (void)
import Data.Typeable

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit hiding (State)
import Test.QuickCheck

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
              (total1, ()) = run $ runState 0 $ sumAll l
              (last1, ()) = run $ runLastWriter $ writeAll l
              (total2, (last2, ())) = run $ runState 0 $ runLastWriter $ writeAndAdd l
              (last3, (total3, ())) = run $ runLastWriter $ runState 0 $ writeAndAdd l
             in allEqual [safeLast l, last1, last2, last3]
           .&&. allEqual [sum l, total1, total2, total3]
  where
    writeAll :: (Typeable a, Member (Writer a) e)
             => [a]
             -> Eff e ()
    writeAll = mapM_ tell

    sumAll :: (Typeable a, Num a, Member (State a) e)
           => [a]
           -> Eff e ()
    sumAll = mapM_ (onState . (+))
    
    writeAndAdd :: (Member (Writer Integer) e, Member (State Integer) e)
                => [Integer]
                -> Eff e ()
    writeAndAdd l = do
        writeAll l
        sumAll l

testCensor :: [Integer] -> Property
testCensor l = property
             $ listE (mapM_ (tell . inc) l) == listE (censor inc $ mapM_ tell l)
  where
    inc :: Integer -> Integer
    inc = (+1)

    listE :: Eff (Writer Integer :> ()) () -> [Integer]
    listE = fst . run . runWriter (:) []

testLastWriterLaziness :: Assertion
testLastWriterLaziness = let (m, ()) = run $ runLastWriter $ mapM_ tell [undefined, ()]
                         in m @?= Just ()

testFirstWriterLaziness :: Assertion
testFirstWriterLaziness = let (m, ()) = run $ runFirstWriter $ mapM_ tell [(), undefined]
                          in m @?= Just ()

tests =
  [ testProperty "Documentation example." testDocs
  , testCase "Test runLastWriter laziness." testLastWriterLaziness
  , testCase "Test runFirstWriter laziness." testFirstWriterLaziness
  ]
