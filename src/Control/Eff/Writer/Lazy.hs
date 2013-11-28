{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Lazy write-only state.
module Control.Eff.Writer.Lazy( Writer
                              , tell
                              , censor
                              , runWriter
                              , runFirstWriter
                              , runLastWriter
                              , runMonoidWriter
                              ) where

import Control.Applicative ((<$>), (<|>))
import Data.Monoid
import Data.Typeable

import Control.Eff

-- | The request to remember a value of type w in the current environment
data Writer w v = Writer w v
    deriving (Typeable, Functor)

-- | Write a new value.
tell :: (Typeable w, Member (Writer w) r) => w -> Eff r ()
tell w = send $ \f -> inj $ Writer w $ f ()

-- | Transform the state being produced.
censor :: (Typeable w, Member (Writer w) r) => (w -> w) -> Eff r a -> Eff r a
censor f = loop . admin
  where
    loop (Val x) = return x
    loop (E u) = interpose u loop
               $ \(Writer w v) -> tell (f w) >> loop v

-- | Handle Writer requests, using a user-provided function to accumulate values.
runWriter :: Typeable w => (w -> b -> b) -> b -> Eff (Writer w :> r) a -> Eff r (b, a)
runWriter accum b = loop . admin
  where
    first f (x, y) = (f x, y)

    loop (Val x) = return (b, x)
    loop (E u) = handleRelay u loop
                 $ \(Writer w v) -> first (accum w) <$> loop v

-- | Handle Writer requests by overwriting previous values.
runLastWriter :: Typeable w => Eff (Writer w :> r) a -> Eff r (Maybe w, a)
runLastWriter = runWriter (\w b -> b <|> Just w) Nothing

-- | Handle Writer requests by taking the first value provided.
runFirstWriter :: Typeable w => Eff (Writer w :> r) a -> Eff r (Maybe w, a)
runFirstWriter = runWriter (\w b -> Just w <|> b) Nothing

-- | Handle Writer requests, using a Monoid instance to accumulate values.
runMonoidWriter :: (Monoid w, Typeable w) => Eff (Writer w :> r) a -> Eff r (w, a)
runMonoidWriter = runWriter (<>) mempty
