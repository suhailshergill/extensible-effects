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
                               , withWriter
                               , tell
                               , censor
                               , runWriter
                               , runFirstWriter
                               , runLastWriter
                               , runListWriter
                               , runMonoidWriter
                               , execWriter
                               , execFirstWriter
                               , execLastWriter
                               , execListWriter
                               , execMonoidWriter
                               ) where

import Control.Eff
import Control.Eff.Extend

import Control.Applicative ((<|>))

import Control.Monad.Base
import Control.Monad.Trans.Control
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

import Data.Function (fix)

-- ------------------------------------------------------------------------
-- | The Writer monad
--
-- In MTL's Writer monad, the told value must have a |Monoid| type. Our
-- writer has no such constraints. If we write a |Writer|-like
-- interpreter to accumulate the told values in a monoid, it will have
-- the |Monoid w| constraint then
data Writer w v where
  Tell :: !w -> Writer w ()

-- | How to interpret a pure value in a writer context, given the
-- value for mempty.
withWriter :: Monad m => a -> b -> (w -> b -> b) -> m (a, b)
withWriter x empty _append = return (x, empty)
-- | Given a value to write, and a callback (which includes empty and
-- append), respond to requests.
instance Monad m => Handle (Writer w) r a (b -> (w -> b -> b) -> m (a, b)) where
  handle step q (Tell w) e append = step (q ^$ ()) e append >>=
    \(x, l) -> return (x, w `append` l)

instance ( MonadBase m m
         , LiftedBase m r
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
censor f = fix (respond_relay' h return)
  where
    h :: (Eff r b -> Eff r b) -> Arrs r v b -> Writer w v -> Eff r b
    h step q (Tell w) = tell (f w) >>= \x -> step (q ^$ x)


-- | Handle Writer requests, using a user-provided function to accumulate
-- values, hence no Monoid constraints.
runWriter :: (w -> b -> b) -> b -> Eff (Writer w ': r) a -> Eff r (a, b)
runWriter accum !b m = fix (handle_relay withWriter) m b accum

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

-- | Handle Writer requests, using a user-provided function to accumulate
--   values and returning the final accumulated values.
execWriter :: (w -> b -> b) -> b -> Eff (Writer w ': r) a -> Eff r b
execWriter accum b = fmap snd . runWriter accum b
{-# INLINE execWriter #-}

-- | Handle Writer requests, using a List to accumulate values and returning
--   the final accumulated values.
execListWriter :: Eff (Writer w ': r) a -> Eff r [w]
execListWriter = fmap snd . runListWriter
{-# INLINE execListWriter #-}

-- | Handle Writer requests, using a Monoid instance to accumulate values and
--   returning the final accumulated values.
execMonoidWriter :: (Monoid w) => Eff (Writer w ': r) a -> Eff r w
execMonoidWriter = fmap snd . runMonoidWriter
{-# INLINE execMonoidWriter #-}

-- | Handle Writer requests by taking the first value provided and and returning
--   the final accumulated values.
execFirstWriter :: Eff (Writer w ': r) a -> Eff r (Maybe w)
execFirstWriter = fmap snd . runFirstWriter
{-# INLINE execFirstWriter #-}

-- | Handle Writer requests by overwriting previous values and returning
--   the final accumulated values.
execLastWriter :: Eff (Writer w ': r) a -> Eff r (Maybe w)
execLastWriter = fmap snd . runLastWriter
{-# INLINE execLastWriter #-}
