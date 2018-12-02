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
instance Handle Trace (IO k) where
  handle k (Trace s) = putStrLn s >> k ()

-- | Print a string as a trace.
trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

-- | Run a computation producing Traces.
-- The handler for IO request: a terminal handler
runTrace :: Eff '[Trace] w -> IO w
runTrace = fix step where
  step next = eff return
              (impureDecomp
                (handle `andThen` next)
                (\_ _ -> error "Impossible: Nothing to relay!"))
