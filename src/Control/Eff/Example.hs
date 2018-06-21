{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators, GADTs, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

-- | Example usage of "Control.Eff"
module Control.Eff.Example where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Exception

import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy

-- {{{ TooBig

-- | The datatype for the example from the paper. See the tests for the example
newtype TooBig = TooBig Int deriving (Eq, Show)

-- | specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

-- }}}

-- | Multiple Reader effects
sum2 :: ([ Reader Int
         , Reader Float
         ] <:: r) => Eff r Float
sum2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1 :: Int)) + (v2 + (2 :: Float))

-- | Write the elements of a list of numbers, in order.
writeAll :: (Member (Writer a) e)
         => [a]
         -> Eff e ()
writeAll = mapM_ tell

-- | Add a list of numbers to the current state.
sumAll :: (Num a, Member (State a) e)
       => [a]
       -> Eff e ()
sumAll = mapM_ (modify . (+))

-- | Write a list of numbers and add them to the current state.
writeAndAdd :: ( [ Writer a
                 , State a
                 ] <:: e
               , Num a)
            => [a]
            -> Eff e ()
writeAndAdd l = do
    writeAll l
    sumAll l

-- | Sum a list of numbers.
sumEff :: (Num a) => [a] -> a
sumEff l = let ((), s) = run $ runState 0 (sumAll l)
           in s

-- | Safely get the last element of a list.
-- Nothing for empty lists; Just the last element otherwise.
lastEff :: [a] -> Maybe a
lastEff l = let ((), a) = run $ runLastWriter $ writeAll l
            in a


-- | Get the last element and sum of a list
lastAndSum :: (Num a) => [a] -> (Maybe a, a)
lastAndSum l = let (((), total), lst) =
                        run $ runLastWriter $ runState 0 (writeAndAdd l)
               in (lst, total)


-- Example by Oscar Key
data Move x where
  Move :: Move ()

handUp :: Eff (Move ': r) a -> Eff r a
handUp (Val x) = return x
handUp (E u q) = case decomp u of
  Right Move -> handDown $ qApp q ()
  -- Relay other requests
  Left u0     -> E u0 ident >>= handUp . qApp q

handDown :: Eff (Move ': r) a -> Eff r a
handDown (Val x) = return x
handDown (E u q) = case decomp u of
  Right Move -> handUp $ qApp q ()
  -- Relay other requests
  Left u0     -> E u0 ident >>= handDown . qApp q
