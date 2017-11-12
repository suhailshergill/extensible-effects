{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
-- | Strict write-only state
module Control.Eff.Writer.Strict1( Writer(..)
                               , tell
                               , censor
                               , runWriter
                               , runFirstWriter
                               , runLastWriter
                               , runListWriter
                               , runMonoidWriter
                               ) where

import Control.Eff1
import Data.OpenUnion51
import Data.FTCQueue1

import Data.Monoid
import Control.Applicative ((<|>))

-- ------------------------------------------------------------------------
-- | The Writer monad
--
-- In MTL's Writer monad, the told value must have a |Monoid| type. Our
-- writer has no such constraints. If we write a |Writer|-like
-- interpreter to accumulate the told values in a monoid, it will have
-- the |Monoid w| constraint then
data Writer w v = Writer !w v

-- | Write a new value.
tell :: Member (Writer w) r => w -> Eff r ()
tell !w = send $ Writer w ()

-- | Transform the state being produced.
censor :: forall w a r. Member (Writer w) r => (w -> w) -> Eff r a -> Eff r a
censor f = interpose return h
  where
    h (Writer w v) k = tell (f w) >> k v


-- | Handle Writer requests, using a user-provided function to accumulate
-- values, hence no Monoid constraints.
runWriter :: (w -> b -> b) -> b -> Eff (Writer w ': r) a -> Eff r (a, b)
runWriter accum !b = handle_relay
  (\x -> return (x, b))
  (\(Writer w v) k -> k v >>= \(x, l) -> return (x, w `accum` l))
  -- the second arg to 'handle_relay' above is same as:
  -- (\(Writer o) k -> second (accum o) `fmap` k ())
  -- where
  --   second f (x, y) = (x, f y)

-- | Handle Writer requests, using a List to accumulate values.
runListWriter :: Eff (Writer w ': r) a -> Eff r (a,[w])
runListWriter = runWriter (:) []

-- | Handle Writer requests, using a Monoid instance to accumulate values.
runMonoidWriter :: (Monoid w) => Eff (Writer w ': r) a -> Eff r (a, w)
runMonoidWriter = runWriter (<>) mempty

-- | Handle Writer requests by taking the first value provided.
runFirstWriter :: Eff (Writer w ': r) a -> Eff r (a, Maybe w)
runFirstWriter = runWriter (\w b -> Just w <|> b) Nothing

-- | Handle Writer requests by overwriting previous values.
runLastWriter :: Eff (Writer w ': r) a -> Eff r (a, Maybe w)
runLastWriter = runWriter (\w b -> b <|> Just w) Nothing
