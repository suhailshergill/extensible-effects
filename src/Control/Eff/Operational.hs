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
-- Define data using GADTs.
--
-- @
--data Jail a where
--    Print :: String -> Jail ()
--    Scan :: Jail String
--
--instance 'Typeable1' Jail where
--    'typeOf1' _ = 'mkTyConApp' ('mkTyCon3' \"test\" \"Main\" \"Jail\") []
--
--prog :: 'Member' ('Program' Jail) r => 'Eff' r ()
--prog = do
--    'singleton' $ Print \"getting input...\"
--    str \<- 'singleton' Scan
--    'singleton' $ Print \"ok\"
--    'singleton' $ Print (\"the input is \" ++ str)
-- @
--
-- Then, implements interpreters from the data to effects.
--
-- @
--adventIO :: ('Member' ('Lift' IO) r, 'SetMember' 'Lift' ('Lift' IO) r) => Jail a -> 'Eff' r a
--adventIO (Print a) = 'lift' $ putStrLn a
--adventIO Scan = 'lift' getLine
--
--adventPure :: ('Member' ('Writer' String) r, 'Member' ('State' [String]) r) => Jail a -> 'Eff' r a
--adventPure (Print a) = 'tell' (a ++ \"\\n\")
--adventPure Scan = do
--    x \<- 'get'
--    case x of
--        [] -\> return []
--        y:ys -\> 'put' ys \>\> return y
-- @
--
-- Use the interpreter.
--
-- @
--main :: IO ()
--main = do
--    putStrLn . fst . 'run' . 'runMonoidWriter' . 'evalState' [\"foo\",\"bar\"] $ 'runProgram' adventPure prog
--    'runLift' $ 'runProgram' adventIO prog
-- @
