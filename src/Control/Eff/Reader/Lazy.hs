{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Lazy read-only state
module Control.Eff.Reader.Lazy( Reader (..)
                              , ask
                              , local
                              , reader
                              , runReader
                              ) where

import Control.Applicative ((<$>))
import Data.Typeable

import Control.Eff

-- | The request for a value of type e from the current environment.
-- This environment is analogous to a parameter of type e.
newtype Reader e v = Reader (e -> v)
    deriving (Typeable, Functor)

-- | Get the current value from a Reader.
ask :: (Typeable e, Member (Reader e) r) => Eff r e
ask = send . inj $ Reader id

-- | Locally rebind the value in the dynamic environment.
-- This function both requests and handles Reader requests.
local :: (Typeable e, Member (Reader e) r)
      => (e -> e)
      -> Eff r a
      -> Eff r a
local f m = do
  e <- f <$> ask
  let loop = freeMap
             return
             (\u -> interpose u loop (\(Reader k) -> loop (k e)))
  loop m

-- | Request the environment value using a transformation function.
reader :: (Typeable e, Member (Reader e) r) => (e -> a) -> Eff r a
reader f = f <$> ask

-- | The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w
runReader m e = loop m
  where
    loop = freeMap
           return
           (\u -> handleRelay u loop (\(Reader k) -> loop (k e)))
