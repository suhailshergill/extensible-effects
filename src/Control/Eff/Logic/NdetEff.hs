{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
-- __TODO__: investigate Fusion regd msplit and associated functions.
module Control.Eff.Logic.NdetEff (
  -- * Main interface
  NdetEff
  , withNdetEff
  , left, right
  , choose
  , makeChoice
  , makeChoiceA
  , module Control.Eff.Logic.Core
    -- * Additional functions for comparison
  , msplit'
  , msplit'_manual
  , makeChoiceA_manual
  , makeChoiceA0
  ) where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Logic.Core
import Control.Eff.Exception

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Function (fix)

-- | An implementation of non-deterministic choice aka backtracking. The two
-- requests we need to support are: @false@, @(|)@. We map this to the
-- 'MonadPlus' (or 'Alternative') interface: @MZero@ stands for @false@, and
-- @MPlus@ stands for @(|)@.
--
-- This creates a branching structure with a fanout of @2@, resulting in @mplus@
-- node being visited approximately @2x@ (in general, for a fanout of @f@ we'll
-- have the type of internal node being invoked @f/(f-1)@ times).
data NdetEff a where
  MZero :: NdetEff a
  MPlus :: NdetEff Bool

-- | How to embed a pure value in non-deterministic context
{-# INLINE withNdetEff #-}
withNdetEff :: Alternative f => Monad m => a -> m (f a)
withNdetEff x = return (pure x)
-- | The left branch
{-# INLINE left #-}
left :: Arrs r Bool a -> Eff r a
left q = q ^$ True
-- | The right branch
{-# INLINE right #-}
right :: Arrs r Bool a -> Eff r a
right q = q ^$ False
-- | Given a callback and NdetEff requests respond to them. Note that this makes
-- explicit that we rely on @f@ to have enough room to store all possibilities.
instance Alternative f => Handle NdetEff r a (Eff r' (f w)) where
  handle _ _ MZero = return empty
  handle step q MPlus = liftM2 (<|>) (step $ left q) (step $ right q)

instance Member NdetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NdetEff r => MonadPlus (Eff r) where
  -- | __NOTE__: the deviation from laws in Control.Monad
  -- [x] mzero >>= f = mzero (CM1)
  -- [ ] m >> mzero  = mzero (CM2)
  -- We obey CM1, but not CM2. This is consistent with Backtr and LogicT papers.
  mzero = send MZero
  -- | Monoid laws (from Haskellwiki, Backtr/LogicT paper)
  -- [x] mzero `mplus` m = m
  -- [x] m `mplus` mzero = m
  -- [x] m `mplus` (n `mplus` o) = (m `mplus` n) `mplus` o
  -- In addition, from Backtr/LogicT we have the following law:
  -- [x] (m `mplus` n) >>= k = (m >>= k) `mplus` (n >>= k)
  mplus (E _ (U0' MZero)) m2 = m2
  mplus m1 (E _ (U0' MZero)) = m1
  -- TODO: verify correctness of above two rules
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (NdetEff ': r)) where
    type StM (Eff (NdetEff ': r)) a = StM (Eff r) [a]
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . makeChoice)
    restoreM x = do lst :: [a] <- raise (restoreM x)
                    choose lst

-- | 'choose' lst non-deterministically chooses one value from the lst choose []
-- thus corresponds to failure.
choose :: Member NdetEff r => [a] -> Eff r a
choose lst = msum $ map return lst

-- | An interpreter: The following is very simple, but leaks a lot of memory The
-- cause probably is mapping every failure to empty It takes then a lot of timne
-- and space to store those empty. When there aren't a lot of failures, this is
-- comparable to 'makeChoiceA'.
makeChoiceA0 :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA0 = fix (handle_relay withNdetEff)

-- | More performant handler; uses reified job queue
instance Alternative f => Handle NdetEff r a ([Eff r a] -> Eff r' (f w)) where
  handle step _ MZero jq = next step jq
  handle step q MPlus jq = next step (left q : right q : jq)
-- instance Handle NdetEff r a (k -> [Eff r a] -> k) where
--   handle step _ MZero z jq = list z (flip step z) jq
--   handle step q MPlus z jq = list z (flip step z) (left q : right q : jq)

{-# INLINE next #-}
-- | Progressing the cursor in a reified job queue.
next :: Alternative f => Monad m
     => (t -> [t] -> m (f a))
     -> [t] -> m (f a)
next k jq = list (return empty) k jq

-- | Optimized version of makeChoiceA, faster and taking less memory. The
-- benefit of the effect framework is that we can have many interpreters.
makeChoiceA :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA m' = loop m' [] where
  loop m = fix (handle_relay @NdetEff ret) m
  -- single result; optimization: drop spurious empty
  ret x [] = withNdetEff x
  -- definite result and perhaps some others
  ret x (h:t) = liftM2 (<|>) (withNdetEff x) (loop h t)

-- | A different implementation, more involved, but similar complexity to
-- makeChoiceA.
makeChoiceA_manual :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA_manual m = loop [] m where
  -- single result; optimization: drop spurious empty
  loop [] (Val x)    = withNdetEff x
  -- definite result and perhaps some others
  loop (h:t) (Val x) = liftM2 (<|>) (withNdetEff x) (loop t h)
  loop jq (E q u) = case decomp u of
    Right MZero     -> next (flip loop) jq
    Right MPlus -> loop (k False : jq) (k True)
    Left  u0 -> relay ((loop jq) . k) u0
    where
      k = (q ^$)

-- | Same as makeChoice, except it has the type hardcoded.
-- Required for MonadBaseControl instance.
makeChoice :: Eff (NdetEff ': r) a -> Eff r [a]
makeChoice = makeChoiceA

-- | We implement LogicT, the non-determinism reflection, of which soft-cut is
-- one instance. See the LogicT paper for an explanation.
instance Member NdetEff r => MSplit (Eff r) where
  msplit = msplit'

-- | Optimized version of msplit'. This is much faster than naive implementation
-- (which takes quadratic time and space when used with sols).
{-# INLINE msplit' #-}
msplit' :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit' m = fix (respond_relay @NdetEff (\x jq -> withMSplit x (msum jq))) m []

-- | A different implementation, more involved, but similar complexity to
-- msplit'.
{-# INLINE msplit'_manual #-}
msplit'_manual :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit'_manual m' = loop m' [] where
  -- definite result and perhaps some others
  loop (Val x) jq = withMSplit x (msum jq)
  -- not yet definite answer
  loop (E q u) jq = case u of
    -- try other choices, if any
    U0' MZero -> next loop jq
    -- try left options; add right to job queue
    U0' MPlus -> loop (k True) (k False : jq)
    _         -> relay (\x -> loop (k x) jq) u
    where
      k x = q ^$ x

-- | The call interpreter -- it is like reify . reflect with a twist. Compare
-- this implementation with the huge implementation of call in Hinze 2000
-- (Figure 9). Each clause corresponds to the axiom of call or cutfalse. All
-- axioms are covered.
--
-- The code clearly expresses the intuition that call watches the choice points
-- of its argument computation. When it encounteres a cutfalse request, it
-- discards the remaining choicepoints.  It completely handles CutFalse effects
-- but not non-determinism
instance Member NdetEff r => Call r where
  call m = loop m [] where
    loop (Val x) jq = return x `mplus` nxt jq          -- (C2)
    loop (E q u) jq = case u of
      U0 (Exc CutFalse) -> nxt []                      -- drop jq (F2)
      U1 u' -> case u' of
        U0' MZero -> nxt jq                            -- (C1)
        U0' MPlus -> nxt (left q : right q : jq)       -- (C3)
        _         -> relay (\x -> loop (q ^$ x) jq) u' -- (C4)

    nxt jq = list mzero loop jq
