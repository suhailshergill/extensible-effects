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
                               , withOperational, Intrprtr (..)
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

-- | General form of an interpreter
newtype Intrprtr f r = Intrprtr { runIntrprtr :: forall x. f x -> Eff r x }

-- | Embed a pure value
withOperational :: a -> Intrprtr f r -> Eff r a
withOperational x _ = return x
-- | Given a continuation and a program, interpret it
instance Handle (Program f) (Intrprtr f r -> Eff r a) where
  handle k (Singleton instr) i = (runIntrprtr i) instr >>= (flip k i)

-- | Lift a value to a monad.
singleton :: (Member (Program instr) r) => instr a -> Eff r a
singleton = send . Singleton

-- | Convert values using given interpreter to effects.
runProgram :: forall f r a. (forall x. f x -> Eff r x) -> Eff (Program f ': r) a -> Eff r a
runProgram advent m = handle_relay withOperational m (Intrprtr advent)

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
