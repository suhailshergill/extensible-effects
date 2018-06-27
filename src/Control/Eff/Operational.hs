{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | Operational Monad (<https://wiki.haskell.org/Operational>) implemented with
-- extensible effects.

module Control.Eff.Operational ( Program (..)
                               , singleton
                               , runProgram
                               -- * Usage
                               -- $usage
                               ) where

import Control.Eff
import Control.Eff.Extend

-- | Lift values to an effect.
-- You can think this is a generalization of @Lift@.
data Program instr v where
  Singleton :: instr a -> Program instr a

-- | Lift a value to a monad.
singleton :: (Member (Program instr) r) => instr a -> Eff r a
singleton = send . Singleton

-- | Convert values using given interpreter to effects.
runProgram :: forall f r a. (forall x. f x -> Eff r x) -> Eff (Program f ': r) a -> Eff r a
runProgram advent = handle_relay return h
  where
    h :: forall v. Program f v -> (v -> Eff r a) -> Eff r a
    h (Singleton instr) k = advent instr >>= k

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
--    let comp = 'runProgram' adventPure prog
--    putStrLn . fst . 'run' . 'runMonoidWriter' $ 'evalState' comp [\"foo\",\"bar\"]
--    'runLift' $ 'runProgram' adventIO prog
-- @
