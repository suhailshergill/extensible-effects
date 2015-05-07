{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
data Program m v = forall a. Program (m a) (a -> v)
#if MIN_VERSION_base(4,7,0)
         deriving (Typeable) -- starting from ghc-7.8 Typeable can only be derived
#else

instance Typeable1 m => Typeable1 (Program m) where
    typeOf1 _ = mkTyConApp (mkTyCon3
                            "extensible-effects"
                            "Control.Eff.Operational"
                            "Program")
                           [typeOf1 (undefined :: m ())]
#endif

instance Functor (Program m) where
    fmap f (Program m k) = Program m (f . k)

-- | Lift a value to a monad.
singleton :: (Typeable1 m, Member (Program m) r) => m a -> Eff r a
singleton m = send . inj $ (Program m) id

-- | Convert values using given interpreter to effects.
runProgram :: Typeable1 f => (forall x. f x -> Eff r x) -> Eff (Program f :> r) a -> Eff r a
runProgram advent = loop where
  loop = freeMap
         return
         (\u -> handleRelay u loop (\ (Program m k) -> advent m >>= loop . k))


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
