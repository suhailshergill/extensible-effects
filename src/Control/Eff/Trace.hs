{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
-- | A Trace effect for debugging
module Control.Eff.Trace( Trace (..)
                        , trace
                        , runTrace
                        ) where

import Data.Typeable

import Control.Eff

-- | Trace effect for debugging
data Trace v = Trace String (() -> v)
    deriving (Typeable, Functor)

-- | Print a string as a trace.
trace :: Member Trace r => String -> Eff r ()
trace x = send . inj $ Trace x id

-- | Run a computation producing Traces.
runTrace :: Eff (Trace :> ()) w -> IO w
runTrace = loop
  where
    loop = freeMap
           return
           (\u -> prjForce u $ \(Trace s k) -> putStrLn s >> loop (k ()))
