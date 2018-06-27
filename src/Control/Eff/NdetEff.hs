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

-- | Another implementation of nondeterministic choice effect
module Control.Eff.NdetEff where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Lift

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

instance Member NdetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NdetEff r => MonadPlus (Eff r) where
  mzero = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
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
makeChoiceA0 = handle_relay (return . pure) $ \m k -> case m of
    MZero -> return empty
    MPlus -> liftM2 (<|>) (k True) (k False)

-- | A different implementation, more involved but faster and taking
-- much less (100 times) less memory.
-- The benefit of the effect framework is that we can have many
-- interpreters.
makeChoiceA :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA m = loop [] m
 where
   loop [] (Val x)    = return (pure x)
   loop (h:t) (Val x) = loop t h >>= \r -> return (pure x <|> r)
   loop jq (E u q) = case  decomp u of
     Right MZero     -> case jq of
       []    -> return empty
       (h:t) -> loop t h
     Right MPlus -> loop (q ^$ False : jq) (q ^$ True)
     Left  u0 -> E u0 (singleK (\x -> loop jq (q ^$ x)))

-- | Same as makeChoiceA, except it has the type hardcoded.
-- Required for MonadBaseControl instance.
makeChoiceLst :: Eff (NdetEff ': r) a -> Eff r [a]
makeChoiceLst = makeChoiceA
-- ------------------------------------------------------------------------
-- Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.

-- We actually implement LogicT, the non-determinism reflection,
-- of which soft-cut is one instance.
-- See the LogicT paper for an explanation
msplit :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit = loop []
 where
 -- singleK result
 loop [] (Val x)  = return (Just (x,mzero))
 -- definite result and perhaps some others
 loop jq (Val x)  = return (Just (x, msum jq))
 -- not yet definite answer
 loop jq (E u q)  = case prj u of
  Just MZero -> case jq of
                   -- no futher choices
                   []     -> return Nothing
                   -- other choices remain, try them
                   (j:jqT) -> loop jqT j
  Just MPlus -> loop ((q ^$ False):jq) (q ^$ True)
  _          -> E u (qComps q (loop jq))

-- Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper
ifte :: Member NdetEff r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = msplit t >>= check
 where check Nothing          = el
       check (Just (sg1,sg2)) = (th sg1) `mplus` (sg2 >>= th)

once :: Member NdetEff r => Eff r a -> Eff r a
once m = msplit m >>= check
 where check Nothing        = mzero
       check (Just (sg1,_)) = return sg1
