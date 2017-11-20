{-# OPTIONS_GHC -Werror #-}

module Utils where

import Control.Exception (ErrorCall, catch)
import Control.Monad

import GHC.IO.Handle
import System.IO
import System.Directory

import Test.HUnit hiding (State)

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

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just $ last l

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)
