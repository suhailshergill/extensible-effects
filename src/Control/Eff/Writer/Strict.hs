{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
-- | Strict write-only state
module Control.Eff.Writer.Strict ( Writer(..)
                               , tell
                               , censor
                               , runWriter
                               , runFirstWriter
                               , runLastWriter
                               , runListWriter
                               , runMonoidWriter
                               ) where

import Control.Eff.Internal
import Data.OpenUnion

import Control.Applicative ((<|>))

import Control.Monad.Base
import Control.Monad.Trans.Control
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

-- ------------------------------------------------------------------------
-- | The Writer monad
--
-- In MTL's Writer monad, the told value must have a |Monoid| type. Our
-- writer has no such constraints. If we write a |Writer|-like
-- interpreter to accumulate the told values in a monoid, it will have
-- the |Monoid w| constraint then
data Writer w v where
  Tell :: !w -> Writer w ()

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
         ) => MonadBaseControl m (Eff (Writer w ': r)) where
    type StM (Eff (Writer w ': r)) a = StM (Eff r) (a, [w])
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . runListWriter)
    restoreM x = do !(a, ws :: [w]) <- raise (restoreM x)
                    mapM_ tell ws
                    return a

-- | Write a new value.
tell :: Member (Writer w) r => w -> Eff r ()
tell !w = send $ Tell w

-- | Transform the state being produced.
censor :: forall w a r. Member (Writer w) r => (w -> w) -> Eff r a -> Eff r a
censor f = interpose return h
  where
    h :: Writer w t -> (t -> Eff r b) -> Eff r b
    h (Tell w) k = tell (f w) >>= k


-- | Handle Writer requests, using a user-provided function to accumulate
-- values, hence no Monoid constraints.
runWriter :: (w -> b -> b) -> b -> Eff (Writer w ': r) a -> Eff r (a, b)
runWriter accum !b = handle_relay
  (\x -> return (x, b))
  (\(Tell w) k -> k () >>= \(x, l) -> return (x, w `accum` l))
  -- the second arg to 'handle_relay' above is same as:
  -- (\(Tell w) k -> second (accum w) `fmap` k ())
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
