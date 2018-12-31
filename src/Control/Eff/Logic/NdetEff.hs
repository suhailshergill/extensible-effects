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
-- __TODO__: export as Control.Eff.Logic
module Control.Eff.Logic.NdetEff (
  NdetEff
  , withNdetEff
  , left, right
  , choose
  , makeChoiceA, makeChoiceA_open
  , makeChoiceA0, makeChoiceA0_open
  , makeChoiceLst
  , msplit0, msplit0_open
  , msplit', msplit'_open
  , sols'
  , module Control.Eff.Logic.Core
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

-- | A different implementation, more directly mapping to MonadPlus
-- interface
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
-- | Given a callback and NdetEff requests respond to them
instance (Alternative f, Monad m) => Handle NdetEff (m (f a)) where
  handle _ MZero = return empty
  handle k MPlus = liftM2 (<|>) (k True) (k False)
-- | Open handler; note that this makes explicit that we rely on @f@ to have
-- enough room to store all possibilities.
instance Alternative f => HandleOpen NdetEff r a (Eff r' (f w)) where
  handle_open _ _ MZero = return empty
  handle_open step q MPlus = liftM2 (<|>) (step $ left q) (step $ right q)

instance Member NdetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NdetEff r => MonadPlus (Eff r) where
  -- | Laws (per Control.Monad)
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
                       f (runInBase . makeChoiceLst)
    restoreM x = do lst :: [a] <- raise (restoreM x)
                    choose lst

-- | choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
choose :: Member NdetEff r => [a] -> Eff r a
choose lst = msum $ map return lst

-- | An interpreter: The following is very simple, but leaks a lot of memory The
-- cause probably is mapping every failure to empty It takes then a lot of timne
-- and space to store those empty. When there aren't a lot of failures, this is
-- comparable to 'makeChoiceA'.
makeChoiceA0 :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA0 = handle_relay withNdetEff
-- | Naive implementation in open-style
makeChoiceA0_open :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA0_open = fix (handle_relay_open withNdetEff)

-- | A different implementation, more involved but faster and taking much less
-- (100 times) less memory. The benefit of the effect framework is that we can
-- have many interpreters.
makeChoiceA :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA m = loop [] m where
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
makeChoiceLst :: Eff (NdetEff ': r) a -> Eff r [a]
makeChoiceLst = makeChoiceA_open

-- | We implement LogicT, the non-determinism reflection, of which soft-cut is
-- one instance. See the LogicT paper for an explanation.
instance Member NdetEff r => MSplit (Eff r) where
  msplit = msplit'_open

-- | Naive implementation
{-# INLINE msplit0 #-}
msplit0 :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit0 = respond_relay (flip withMSplit empty) $ \k x -> case x of
  MZero -> return Nothing              -- definite failure
  MPlus -> k True >>= \r -> case r of  -- check left first
    Nothing -> k False                 -- failure, continue exploring
    Just(a, m) -> withMSplit a (m <|> (k False >>= reflect)) -- definite success
-- | Naive open implementation. Note this forgets everything but the first result
{-# INLINE msplit0_open #-}
msplit0_open :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit0_open = fix (respond_relay_open @NdetEff (flip withMSplit empty))

-- | A different implementation, more involved. This is much faster than naive
-- implementation (which takes quadratic time and space when used with sols).
{-# INLINE msplit' #-}
msplit' :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit' m' = loop m' [] where
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

{-# INLINE next #-}
-- | Progressing the cursor in a reified job queue.
next :: Alternative f => Monad m
     => (t -> [t] -> m (f a))
     -> [t] -> m (f a)
next k jq = list (return empty) k jq

-- instance HandleOpen NdetEff r a (k -> [Eff r a] -> k) where
--   handle_open step _ MZero z jq = list z (flip step z) jq
--   handle_open step q MPlus z jq = list z (flip step z) (left q : right q : jq)

instance Alternative f => HandleOpen NdetEff r a ([Eff r a] -> Eff r' (f w)) where
  handle_open step _ MZero jq = next step jq
  handle_open step q MPlus jq = next step (left q : right q : jq)

-- | Alternate implementation of makeChoiceA. Slightly faster than makeChoiceA.
makeChoiceA_open :: Alternative f
                 => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA_open m' = loop m' [] where
  loop m = handle_relay_open @NdetEff
           ret loop m
  -- single result; optimization: drop spurious empty
  ret x [] = withNdetEff x
  -- definite result and perhaps some others
  ret x (h:t) = liftM2 (<|>) (withNdetEff x) (loop h t)

-- | Alternate implementation of msplit'. Just as good as msplit' (allocates an
-- extra 24 bytes ;)).
{-# INLINE msplit'_open #-}
msplit'_open :: Member NdetEff r
    => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit'_open m' = loop m' [] where
  loop m = respond_relay_open @NdetEff
           (\x jq -> withMSplit x (msum jq))
           loop m

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

-- | Direct implementation of sols. This currently takes about 6.7% less
-- memory. Fusion techniques would likely help here.
--
-- __NOTES__: This traverses choices twice; once to build job queue, then to
-- process it.
--
-- __TODO__: decide whether to retain this or remove
sols' :: Member NdetEff r => Eff r a -> Eff r [a]
sols' m = loop [] m where
  loop [] (Val x)    = withNdetEff x
  loop (h:t) (Val x) = liftM2 (<|>) (withNdetEff x) (loop t h)
  loop jq (E q u) = case u of
    U0' MZero -> next (flip loop) jq
    U0' MPlus -> loop (k False : jq) (k True)
    _         -> relay ((loop jq) . k) u
    where
      k = (q ^$)
