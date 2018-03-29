{-# OPTIONS_GHC -Werror #-}

module Utils where

import Control.Exception (ErrorCall, catch)
import Control.Monad
import Control.Monad.Trans.Control

import System.IO.Silently
import Data.Tuple (swap)

import Test.HUnit hiding (State)

-- | capture stdout
-- [[https://stackoverflow.com/a/11128420][source]]
catchOutput :: IO a -> IO (a, String)
catchOutput f = swap `fmap` capture f

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

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just $ last l

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

doThing :: MonadBaseControl b m => m a -> m a
doThing = liftBaseOp_ go
  where
    go :: Monad m => m a -> m a
    go a = return () >> a
