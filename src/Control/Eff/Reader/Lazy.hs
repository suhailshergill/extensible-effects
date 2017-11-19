{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
-- | Lazy read-only state
module Control.Eff.Reader.Lazy ( Reader (..)
                              , ask
                              , local
                              , reader
                              , runReader
                              ) where

import Control.Eff
import Data.OpenUnion

-- ------------------------------------------------------------------------
-- | The Reader monad
--
-- The request for a value of type e from the current environment
-- This can be expressed as a GADT because the type of values
-- returned in response to a (Reader e a) request is not any a;
-- we expect in reply the value of type 'e', the value from the
-- environment. So, the return type is restricted: 'a ~ e'
-- data Reader e v where
--   Reader :: Reader e e
--
-- One can also define this as
--    data Reader e v = (e ~ v) => Reader
-- and even without GADTs, using explicit coercion as is done here.
newtype Reader e v = Reader (e->v)
-- ^ In the latter case, when we make the request, we make it as Reader id.
-- So, strictly speaking, GADTs are not really necessary.


-- | Get the current value from a Reader.
-- The signature is inferred (when using NoMonomorphismRestriction).
ask :: (Member (Reader e) r) => Eff r e
ask = send $ Reader id

-- | The handler of Reader requests. The return type shows that all Reader
-- requests are fully handled.
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handle_relay
  return
  (\(Reader f) k -> k (f e))
  m

-- | Locally rebind the value in the dynamic environment This function is like a
-- relay; it is both an admin for Reader requests, and a requestor of them.
-- The underscore is used to disable name-shadowing warning.
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local _f m = do
  e <- reader _f
  let h (Reader _f) g = g (_f e)
  interpose return h m

-- | Request the environment value using a transformation function.
reader :: (Member (Reader e) r) => (e -> a) -> Eff r a
reader f = f `fmap` ask
