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
-- The following is needed for pattern-synonym bug in ghc 8.2
{-# LANGUAGE CPP #-}

-- | Nondeterministic choice effect via MPlus interface directly. In order to
-- get an understanding of what nondeterministic choice entails the following
-- papers are recommended:
--
-- * [@LogicT@] [LogicT - backtracking monad transformer with fair operations and pruning](http://okmij.org/ftp/Computation/monads.html#LogicT)
-- * [@Backtr@] [Deriving Backtracking Monad Transformers](https://dl.acm.org/citation.cfm?id=351240.351258)
--
-- __TODO__: investigate Fusion regd msplit and associated functions.
module Control.Eff.Logic.NDet (
  -- * Main interface
  NDet
  , withNDet
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
data NDet a where
  MZero :: NDet a
  MPlus :: NDet Bool

-- | How to embed a pure value in non-deterministic context
{-# INLINE withNDet #-}
withNDet :: Alternative f => Monad m => a -> m (f a)
withNDet x = return (pure x)
-- | The left branch
{-# INLINE left #-}
left :: Arrs r Bool a -> Eff r a
left q = q ^$ True
-- | The right branch
{-# INLINE right #-}
right :: Arrs r Bool a -> Eff r a
right q = q ^$ False
-- | Given a callback and 'NDet' requests respond to them. Note that this makes
-- explicit that we rely on @f@ to have enough room to store all possibilities.
instance Alternative f => Handle NDet r a (Eff r' (f w)) where
  handle _ _ MZero = return empty
  handle h q MPlus = liftM2 (<|>) (h $ left q) (h $ right q)

instance Member NDet r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

-- | Mapping of 'NDet' requests to 'MonadPlus'. We obey the following laws
-- (taken from the @Backtr@ and @LogicT papers):
--
-- > mzero >>= f = mzero                               -- (L1)
-- > mzero `mplus` m = m                               -- (L2)
-- > m `mplus` mzero = m                               -- (L3)
-- > m `mplus` (n `mplus` o) = (m `mplus` n) `mplus` o -- (L4)
-- > (m `mplus` n) >>= k = (m >>= k) `mplus` (n >>= k) -- (L5)
--
-- - @L1@ is the left-zero law for 'mzero'
-- - @L2, L3, L4@ are the @Monoid@ laws
--
-- __NOTE__ that we do __not__ obey the right-zero law for
-- 'mzero'. Specifically, we do __not__ obey:
--
-- > m >> mzero  = mzero
instance Member NDet r => MonadPlus (Eff r) where
  mzero = send MZero
  -- | Applying L2 and L3
#if __GLASGOW_HASKELL__ < 804
  mplus (E _ u) m2 | Just MZero <- prj u = m2
  mplus m1 (E _ u) | Just MZero <- prj u = m1
#else
  mplus (E _ (U0' MZero)) m2 = m2
  mplus m1 (E _ (U0' MZero)) = m1
#endif
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (NDet ': r)) where
    type StM (Eff (NDet ': r)) a = StM (Eff r) [a]
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . makeChoice)
    restoreM x = do lst :: [a] <- raise (restoreM x)
                    choose lst

-- | @'choose' lst@ non-deterministically chooses one value from the
-- @lst@. @'choose' []@ thus corresponds to failure.
choose :: Member NDet r => [a] -> Eff r a
choose lst = msum $ map return lst

-- | An interpreter: The following is very simple, but leaks a lot of memory The
-- cause probably is mapping every failure to empty It takes then a lot of timne
-- and space to store those empty. When there aren't a lot of failures, this is
-- comparable to 'makeChoiceA'.
makeChoiceA0 :: Alternative f => Eff (NDet ': r) a -> Eff r (f a)
makeChoiceA0 = fix (handle_relay withNDet)

-- | More performant handler; uses reified job queue
instance Alternative f => Handle NDet r a ([Eff r a] -> Eff r' (f w)) where
  handle h _ MZero jq = next h jq
  handle h q MPlus jq = next h (left q : right q : jq)
-- instance Handle NDet r a (k -> [Eff r a] -> k) where
--   handle h _ MZero z jq = list z (flip h z) jq
--   handle h q MPlus z jq = list z (flip h z) (left q : right q : jq)

{-# INLINE next #-}
-- | Progressing the cursor in a reified job queue.
next :: Alternative f => Monad m
     => (t -> [t] -> m (f a))
     -> [t] -> m (f a)
next k jq = list (return empty) k jq

-- | Optimized implementation, faster and taking less memory. The benefit of the
-- effect framework is that we can have many interpreters.
makeChoiceA :: Alternative f => Eff (NDet ': r) a -> Eff r (f a)
makeChoiceA m' = loop m' [] where
  loop m = fix (handle_relay @NDet ret) m
  -- single result; optimization: drop spurious empty
  ret x [] = withNDet x
  -- definite result and perhaps some others
  ret x (h:t) = liftM2 (<|>) (withNDet x) (loop h t)

-- | A different implementation, more involved, but similar complexity to
-- 'makeChoiceA'.
makeChoiceA_manual :: Alternative f => Eff (NDet ': r) a -> Eff r (f a)
makeChoiceA_manual m = loop m [] where
  -- single result; optimization: drop spurious empty
  loop (Val x) []    = withNDet x
  -- definite result and perhaps some others
  loop (Val x) (h:t) = liftM2 (<|>) (withNDet x) (loop h t)
  loop (E q u) jq    = case decomp u of
    Right MZero -> next loop jq
    Right MPlus -> loop (k True) (k False : jq)
    Left  u0    -> relayK (loop . k) u0 jq
    where
      k = (q ^$)

-- | Same as 'makeChoiceA', except it has the type hardcoded.
-- Required for 'MonadBaseControl' instance.
makeChoice :: Eff (NDet ': r) a -> Eff r [a]
makeChoice = makeChoiceA

-- | We implement LogicT, the non-determinism reflection, of which soft-cut is
-- one instance. See the LogicT paper for an explanation.
instance Member NDet r => MSplit (Eff r) where
  msplit = msplit'

-- | The implementation of 'MSplit'. Exported as a standalone to make
-- testing/comparison easier.
{-# INLINE msplit' #-}
msplit' :: Member NDet r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit' m = fix (respond_relay @NDet (\x jq -> withMSplit x (msum jq))) m []

-- | A different implementation, more involved, but similar complexity to
-- 'msplit''.
{-# INLINE msplit'_manual #-}
msplit'_manual :: Member NDet r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit'_manual m' = loop m' [] where
  -- definite result and perhaps some others
  loop (Val x) jq = withMSplit x (msum jq)
  -- not yet definite answer
  loop (E q u) jq = case u of
    -- try other choices, if any
    U0' MZero -> next loop jq
    -- try left options; add right to job queue
    U0' MPlus -> loop (k True) (k False : jq)
    _         -> relayK (loop . k) u jq
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
instance Member NDet r => Call r where
  call m = loop m [] where
    loop (Val x) jq = return x `mplus` nxt jq          -- (C2)
    loop (E _ (U0 (Exc CutFalse))) _ = nxt []          -- drop jq (F2)
    loop (E q (U1 u)) jq = case u of
        U0' MZero -> nxt jq                            -- (C1)
        U0' MPlus -> nxt (left q : right q : jq)       -- (C3)
        _         -> relay loop q u jq                 -- (C4)

    nxt jq = list mzero loop jq
