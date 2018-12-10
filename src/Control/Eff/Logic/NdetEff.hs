{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

-- | Nondeterministic choice effect via MPlus interface directly
module Control.Eff.Logic.NdetEff (
  NdetEff
  , withNdetEff
  , left, right
  , makeChoiceA
  , makeChoiceA0
  , makeChoiceLst
  , msplit1
  , module Control.Eff.Logic.Core
  ) where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Logic.Core

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Foldable (foldl')

-- | A different implementation, more directly mapping to MonadPlus
-- interface
data NdetEff a where
  MZero :: NdetEff a
  MPlus :: NdetEff Bool

-- | How to embed a pure value in non-deterministic context
withNdetEff :: Alternative f => Monad m => a -> m (f a)
withNdetEff = return . pure
-- | The left branch
left :: (Bool -> k) -> k
left k = k True
-- | The right branch
right :: (Bool -> k) -> k
right k = k False
-- | Given a callback and NdetEff requests respond to them
instance (Alternative f, Monad m) => Handle NdetEff (m (f a)) where
  handle _ MZero = return empty
  handle k MPlus = liftM2 (<|>) (left k) (right k)

instance Member NdetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NdetEff r => MonadPlus (Eff r) where
  mzero = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (NdetEff ': r)) where
    type StM (Eff (NdetEff ': r)) a = StM (Eff r) [a]
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . makeChoiceLst)
    restoreM x = do lst :: [a] <- raise (restoreM x)
                    foldl' (\r a -> r <|> pure a) mzero lst

-- | An interpreter
-- The following is very simple, but leaks a lot of memory
-- The cause probably is mapping every failure to empty
-- It takes then a lot of timne and space to store those empty
makeChoiceA0 :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA0 = handle_relay withNdetEff

-- | A different implementation, more involved but faster and taking
-- much less (100 times) less memory.
-- The benefit of the effect framework is that we can have many
-- interpreters.
makeChoiceA :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA m = loop [] m where
  loop [] (Val x)    = withNdetEff x
  loop (h:t) (Val x) = liftM2 (<|>) (withNdetEff x) (loop t h)
  loop jq (E q u) = case  decomp u of
    Right MZero     -> case jq of
      []    -> return empty
      (h:t) -> loop t h
    Right MPlus -> loop (q ^$ False : jq) (q ^$ True)
    Left  u0 -> E (q ^|$^ (loop jq)) u0

-- | Same as makeChoiceA, except it has the type hardcoded.
-- Required for MonadBaseControl instance.
makeChoiceLst :: Eff (NdetEff ': r) a -> Eff r [a]
makeChoiceLst = makeChoiceA

-- | We actually implement LogicT, the non-determinism reflection, of
-- which soft-cut is one instance. Straightforward implementation
-- using 'respond_relay'. See the LogicT paper for an explanation.
instance Member NdetEff r => MSplit (Eff r) where
  msplit = respond_relay (flip withMSplit empty) $ \k x -> case x of
    MZero -> return Nothing              -- definite failure
    MPlus -> left k >>= \r -> case r of  -- check left first
      Nothing -> right k                 -- failure, continue exploring
      Just(a, m) -> withMSplit a (m <|> (right k >>= reflect)) -- definite success

-- | A different implementation, more involved. Unclear whether this
-- is faster or not.
msplit1 :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit1 = loop []
 where
 -- single result
 loop [] (Val x)  = withMSplit x mzero
 -- definite result and perhaps some others
 loop jq (Val x)  = withMSplit x (msum jq)
 -- not yet definite answer
 loop jq (E q u)  = case prj u of
  Just MZero -> case jq of
                   -- no futher choices
                   []     -> return Nothing
                   -- other choices remain, try them
                   (j:jqT) -> loop jqT j
  Just MPlus -> loop ((q ^$ False):jq) (q ^$ True)
  _          -> E (q ^|$^ (loop jq)) u
