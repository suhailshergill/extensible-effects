{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Create unique Enumerable values.
module Control.Eff.Fresh( Fresh (..)
                        , fresh
                        , runFresh
                        ) where

import Data.Typeable

import Control.Eff

-- | Create unique Enumerable values.
newtype Fresh i v = Fresh (i -> v)
    deriving (Functor, Typeable)

-- | Produce a value that has not been previously produced.
fresh :: (Typeable i, Enum i, Member (Fresh i) r) => Eff r i
fresh = send (inj . Fresh)

-- | Run an effect requiring unique values.
runFresh :: (Typeable i, Enum i) => Eff (Fresh i :> r) w -> i -> Eff r w
runFresh m s0 = loop s0 (admin m)
  where
    loop _ (Val x) = return x
    loop s (E u)   = handleRelay u (loop s) $
                          \(Fresh k) -> (loop $! succ s) (k s)
