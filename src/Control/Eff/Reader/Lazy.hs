{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
-- | Lazy read-only state
module Control.Eff.Reader.Lazy ( Reader (..)
                              , ask
                              , local
                              , reader
                              , runReader
                              ) where

import Control.Eff
import Control.Eff.Extend

import Control.Monad.Base
import Control.Monad.Trans.Control

-- ------------------------------------------------------------------------
-- | The Reader monad
--
-- The request for a value of type e from the current environment
-- This can be expressed as a GADT because the type of values
-- returned in response to a (Reader e a) request is not any a;
-- we expect in reply the value of type 'e', the value from the
-- environment. So, the return type is restricted: 'a ~ e'
data Reader e v where
  Ask :: Reader e e
-- ^
-- One can also define this as
--
-- @
-- data Reader e v = (e ~ v) => Reader
-- @
-- ^ without GADTs, using explicit coercion as is done here.
--
-- @
-- newtype Reader e v = Reader (e->v)
-- @
-- ^ In the latter case, when we make the request, we make it as Reader id.
-- So, strictly speaking, GADTs are not really necessary.


-- | Get the current value from a Reader.
-- The signature is inferred (when using NoMonomorphismRestriction).
ask :: (Member (Reader e) r) => Eff r e
ask = send Ask

-- | The handler of Reader requests. The return type shows that all Reader
-- requests are fully handled.
runReader :: e -> Eff (Reader e ': r) w -> Eff r w
runReader e = handle_relay
  return
  (\k Ask -> k e)

-- | Locally rebind the value in the dynamic environment This function is like a
-- relay; it is both an admin for Reader requests, and a requestor of them.
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e <- reader f
  let
    h :: (t -> Eff r b) -> Reader e t -> Eff r b
    h k Ask = k e
  interpose return h m

-- | Request the environment value using a transformation function.
reader :: (Member (Reader e) r) => (e -> a) -> Eff r a
reader f = f `fmap` ask

instance ( MonadBase m m
         , LiftedBase m s
         ) => MonadBaseControl m (Eff (Reader e ': s)) where
    type StM (Eff (Reader e ': s)) a = StM (Eff s) a
    liftBaseWith f = do e <- ask
                        raise $ liftBaseWith $ \runInBase ->
                          f (runInBase . runReader e)
    restoreM = raise . restoreM
