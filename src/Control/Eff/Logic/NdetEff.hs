{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
  , makeChoiceA1
  , makeChoiceLst
  , msplit2, sols'
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
deriving instance Show a => Show (NdetEff a)

-- | How to embed a pure value in non-deterministic context
{-# INLINE withNdetEff #-}
withNdetEff :: Alternative f => Monad m => a -> m (f a)
withNdetEff x = return (pure x)
-- | The left branch
{-# INLINE left #-}
left :: (Bool -> k) -> k
left k = k True
-- | The right branch
{-# INLINE right #-}
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

-- | An interpreter: The following is very simple, but leaks a lot of memory The
-- cause probably is mapping every failure to empty It takes then a lot of timne
-- and space to store those empty. When there aren't a lot of failures, this is
-- comparable to 'makeChoiceA'.
makeChoiceA0 :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA0 = handle_relay withNdetEff

-- | A different implementation, more involved but faster and taking much less
-- (100 times) less memory. The benefit of the effect framework is that we can
-- have many interpreters.
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

-- Can we abstract the pattern in the more performant 'makeChoiceA'? Note that
-- this utilizes a Handle' instance instead of a Handle instance.

-- | Handle with aid of a delegate which operates on reified queue
instance Handle' NdetEff b (([b] -> k) -> ([b] -> k)) where
  -- when no result, call dlgt directly
  handle' _ MZero dlgt jq = dlgt jq
  -- otherwise, add left and right branches to job queue before calling dlgt
  handle' k MPlus dlgt jq = dlgt (left k : right k : jq)

-- | Case analysis for lists
list :: b -> (a -> [a] -> b)
     -> [a] -> b
list z _ [] = z
list _ k (h:t) = k h t

{-# INLINE next #-}
-- | Progressing the cursor in a job queue.
next :: Alternative f => Monad m
     => (t -> [t] -> m (f a))
     -> [t] -> m (f a)
next k = list (return empty) k

-- | Alternate implementation of makeChoiceA
makeChoiceA1 :: Alternative f
             => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA1 m' = loop m' [] where
  -- step could instead be passed to handle_relay'' directly, but then GHC
  -- doesn't inline
  loop m = handle_relay'' (\x -> ret x step)
           (\k x -> handle' k x step) m
  -- single result
  ret x _    [] = withNdetEff x -- optimization: drop spurious empty
  -- definite result and perhaps some others
  ret x dlgt jq = liftM2 (<|>) (withNdetEff x) (dlgt jq)
  step = next loop

-- | Same as makeChoiceA, except it has the type hardcoded.
-- Required for MonadBaseControl instance.
makeChoiceLst :: Eff (NdetEff ': r) a -> Eff r [a]
makeChoiceLst = makeChoiceA

-- | We actually implement LogicT, the non-determinism reflection, of
-- which soft-cut is one instance. Straightforward implementation
-- using 'respond_relay'. See the LogicT paper for an explanation.
instance Member NdetEff r => MSplit (Eff r) where
  msplit = msplit1

-- | A different implementation, more involved. This is much faster than naive
-- implementation (which takes quadratic time and space when used with sols).
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
  Just MPlus -> loop (q ^$ False : jq) (q ^$ True)
  _          -> E (q ^|$^ (loop jq)) u

-- | A less involved implementation utilizing Handle' instance. Algorithmic
-- complexity is similar to 'msplit1', but slightly less performant.
-- __TODO__: investigate
msplit2 :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit2 m' = loop m' [] where
  -- respond_relay'' needs to be 'fully applied' to make use of inlining
  loop m = respond_relay'' ret
           (\k x -> handle' @NdetEff k x (next loop)) m
  -- single result
  ret x [] = withMSplit x mzero -- avoids call to msum/foldr; no great benefit
  -- definite result and perhaps some others
  ret x jq = withMSplit x (msum jq)

-- | Direct implementation of sols. This currently is about 3x faster than the
-- implementation based on 'msplit'.
-- __TODO__: investigate reason and fix
sols' :: Member NdetEff r => Eff r a -> Eff r [a]
sols' m = loop [] m where
  loop [] (Val x)    = withNdetEff x
  loop (h:t) (Val x) = liftM2 (<|>) (withNdetEff x) (loop t h)
  loop jq (E q u) = case u of
    U0' MZero -> case jq of
      []    -> return empty
      (h:t) -> loop t h
    U0' MPlus -> loop (q ^$ False : jq) (q ^$ True)
    _         -> E (q ^|$^ (loop jq)) u
-- sols' = respond_relay' @NdetEff withNdetEff
{-
sols' m' = loop m' [] where
  -- step could instead be passed to handle_relay'' directly, but then GHC
  -- doesn't inline
  loop m = respond_relay'' (\x -> ret x step)
           (\k x -> handle' @NdetEff k x step) m
  -- single result
  ret x _    [] = withNdetEff x -- optimization: drop spurious empty
  -- definite result and perhaps some others
  ret x dlgt jq = liftM2 (<|>) (withNdetEff x) (dlgt jq)
  step = next loop
-}
