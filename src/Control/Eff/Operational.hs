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
#endif

-- | Operational Monad (<https://wiki.haskell.org/Operational>) implemented with
-- extensible effects.

module Control.Eff.Operational ( Program (..)
                               , singleton
                               , runProgram
                               -- * Usage
                               -- $usage
                               ) where

import Data.Typeable
import Control.Eff

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#endif

-- | Lift values to an effect.
-- You can think this is a generalization of @Lift@.
data Program instr v = forall a. Program (instr a) (a -> v)
#if MIN_VERSION_base(4,7,0)
         deriving (Typeable) -- starting from ghc-7.8 Typeable can only be derived
#else

instance Typeable1 instr => Typeable1 (Program instr) where
    typeOf1 _ = mkTyConApp (mkTyCon3
                            "extensible-effects"
                            "Control.Eff.Operational"
                            "Program")
                           [typeOf1 (undefined :: instr ())]
#endif

instance Functor (Program instr) where
    fmap f (Program instr k) = Program instr (f . k)

-- | Lift a value to a monad.
singleton :: (Typeable1 instr, Member (Program instr) r) => instr a -> Eff r a
singleton instr = send . inj $ (Program instr) id

-- | Convert values using given interpreter to effects.
runProgram :: Typeable1 f => (forall x. f x -> Eff r x) -> Eff (Program f :> r) a -> Eff r a
runProgram advent = loop where
  loop = freeMap
         return
         (\u -> handleRelay u loop (\ (Program instr k) -> advent instr >>= loop . k))


-- $usage
--
-- See 'Control.Eff.Operational.Example' for an example of defining data using
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
