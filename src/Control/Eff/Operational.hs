{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ < 708
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif

-- | Operational Monad (<https://wiki.haskell.org/Operational>) implemented with
-- extensible effects.

module Control.Eff.Operational ( Program (..)
                               , singleton
                               , runProgram
                               -- * Usage
                               -- $usage
                               ) where

import Control.Eff
import Data.OpenUnion

-- | Lift values to an effect.
-- You can think this is a generalization of @Lift@.
data Program instr v = forall a. Program (instr a) (a -> v)

-- | Lift a value to a monad.
singleton :: (Member (Program instr) r) => instr a -> Eff r a
singleton instr = send $ (Program instr) id

-- | Convert values using given interpreter to effects.
runProgram :: (forall x. f x -> Eff r x) -> Eff (Program f ': r) a -> Eff r a
runProgram advent = handle_relay return h
  where
    h (Program instr v) k = advent instr >>= k . v

-- $usage
--
-- See "Control.Eff.Operational.Example" for an example of defining data using
-- GADTs and implementing interpreters from the data to effects.
--
-- To use the interpreter, see below or consult the tests.
--
-- @
--main :: IO ()
--main = do
--    putStrLn . fst . 'run' . 'runMonoidWriter' . 'evalState' [\"foo\",\"bar\"] $ 'runProgram' adventPure prog
--    'runLift' $ 'runProgram' adventIO prog
-- @
