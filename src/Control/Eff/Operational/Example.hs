{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | Example usage of "Control.Eff.Operational".
module Control.Eff.Operational.Example where

import Control.Eff.Operational
import Control.Eff (Eff(..))
import Data.OpenUnion
import Control.Eff.Lift
import Control.Eff.Writer.Lazy
import Control.Eff.State.Lazy

-- | Define data using GADTs.
data Jail a where
   Print :: String -> Jail ()
   Scan :: Jail String

prog :: Member (Program Jail) r => Eff r ()
prog = do
   singleton $ Print "getting input..."
   str <- singleton Scan
   singleton $ Print "ok"
   singleton $ Print ("the input is " ++ str)

-- | Then, implements interpreters from the data to effects.
adventIO :: (SetMember Lift (Lift IO) r) => Jail a -> Eff r a
adventIO (Print a) = lift $ putStrLn a
adventIO Scan = lift getLine

adventPure :: (Member (Writer String) r, Member (State [String]) r) => Jail a -> Eff r a
adventPure (Print a) = tell (a ++ "\n")
adventPure Scan = do
  x <- get
  case x of
    [] -> return []
    y:ys -> put ys >> return y
