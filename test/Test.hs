{-# LANGUAGE FlexibleContexts #-}
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State

import Control.Monad (void)
import Data.Typeable

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2

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
              (last1, ()) = run $ runWriter $ writeAll l
              (total2, (last2, ())) = run $ runState 0 $ runWriter $ writeAndAdd l
              (last3, (total3, ())) = run $ runWriter $ runState 0 $ writeAndAdd l
             in allEqual [safeLast l, last1, last2, last3]
           .&&. allEqual [sum l, total1, total2, total3]
  where
    writeAll :: (Typeable a, Member (Writer a) e)
             => [a]
             -> Eff e ()
    writeAll = mapM_ putWriter

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

tests = [
    testProperty "Documentation example." testDocs
  ]
