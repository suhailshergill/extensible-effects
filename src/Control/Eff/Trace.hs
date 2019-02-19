{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE Safe #-}
-- | A Trace effect for debugging
module Control.Eff.Trace( Trace (..)
                        , withTrace
                        , trace
                        , runTrace
                        ) where

import Control.Eff
import Control.Eff.Extend
import Data.Function (fix)

-- | Trace effect for debugging
data Trace v where
  Trace :: String -> Trace ()

-- | Embed a pure value in Trace context
withTrace :: a -> IO a
withTrace = return

-- | Given a callback and request, respond to it
instance Handle Trace r a (IO k) where
  handle h q (Trace s) = putStrLn s >> h (q ^$ ())

-- | Print a string as a trace.
trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

-- | Run a computation producing Traces.
-- The handler for IO request: a terminal handler
runTrace :: Eff '[Trace] w -> IO w
runTrace = fix h where
  h next = eff return
              (\q u -> case u of
                  U0 x -> handle next q x
                  _    -> error "Impossible: Nothing to relay!")
