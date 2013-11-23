{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Exception-producing and exception-handling effects
module Control.Eff.Exception( Exc (..)
                            , throwExc
                            , runExc
                            , catchExc
                            ) where

import Data.Typeable

import Control.Eff

-- | These are exceptions of the type e. This is akin to the error monad.
newtype Exc e v = Exc e
    deriving (Functor, Typeable)

-- | Throw an exception in an effectful computation.
throwExc :: (Typeable e, Member (Exc e) r) => e -> Eff r a
throwExc e = send (\_ -> inj $ Exc e)

-- | Run a computation that might produce an exception.
runExc :: Typeable e => Eff (Exc e :> r) a -> Eff r (Either e a)
runExc m = loop (admin m)
 where
  loop (Val x)  = return (Right x)
  loop (E u)    = handleRelay u loop (\(Exc e) -> return (Left e))

-- | Run a computation that might produce exceptions,
-- and give it a way to deal with the exceptions that come up.
catchExc :: (Typeable e, Member (Exc e) r)
         => Eff r a
         -> (e -> Eff r a)
         -> Eff r a
catchExc m handle = loop (admin m)
 where
  loop (Val x)  = return x
  loop (E u)    = interpose u loop (\(Exc e) -> handle e)
