{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Logic primitives. See @LogicT@ paper for details.
--
-- * [@LogicT@] [LogicT - backtracking monad transformer with fair operations and pruning](http://okmij.org/ftp/Computation/monads.html#LogicT)
module Control.Eff.Logic.Core where

import Control.Monad

import Control.Eff
import Control.Eff.Exception

import Data.Function (fix)

-- | The MSplit primitive from LogicT paper.
class MSplit m where
  -- | The laws for 'msplit' are:
  --
  -- > msplit mzero                = return Nothing
  -- > msplit (return a `mplus` m) = return (Just(a, m))
  msplit :: m a -> m (Maybe (a, m a))

-- | Embed a pure value into MSplit
{-# INLINE withMSplit #-}
withMSplit :: MonadPlus m => a -> m a -> m (Maybe (a, m a))
withMSplit a rest = return (Just (a, rest))
-- The handlers are defined in terms of the specific non-determinism
-- effects (instead of by way of a distinct MSplit handler

-- | Laws for 'reflect':
--
-- > msplit (lift m >> mzero)   >>= reflect = lift m >> mzero
-- > msplit (lift m `mplus` ma) >>= reflect = lift m `mplus` (msplit ma >>= reflect)
{-# INLINE reflect #-}
reflect :: MonadPlus m => Maybe (a, m a) -> m a
reflect Nothing      = mzero
reflect (Just (a,m)) = return a `mplus` m

-- Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper

-- | Soft-cut: non-deterministic if-then-else, aka Prolog's @*->@
-- Declaratively,
--
-- >  ifte t th el = (t >>= th) `mplus` ((not t) >> el)
--
-- However, @t@ is evaluated only once. In other words, @ifte t th el@
-- is equivalent to @t >>= th@ if @t@ has at least one solution.
-- If @t@ fails, @ifte t th el@ is the same as @el@. Laws:
--
-- > ifte (return a) th el           = th a
-- > ifte mzero th el                = el
-- > ifte (return a `mplus` m) th el = th a `mplus` (m >>= th)
ifte :: (MonadPlus m, MSplit m)
     => m t -> (t -> m b) -> m b -> m b
ifte t th el = msplit t >>= check
 where check Nothing          = el
       check (Just (sg1,sg2)) = (th sg1) `mplus` (sg2 >>= th)

-- | Another pruning operation (ifte is the other). This selects one
-- solution out of possibly many.
once :: (MSplit m, MonadPlus m) => m b -> m b
once m = msplit m >>= check
 where check Nothing        = mzero
       check (Just (sg1,_)) = return sg1

-- | Negation as failure
gnot :: (MonadPlus m, MSplit m) => m b -> m ()
gnot m = ifte (once m) (const mzero) (return ())

-- | Fair (i.e., avoids starvation) disjunction. It obeys the
-- following laws:
--
-- > interleave mzero m                  = m
-- > interleave (return a `mplus` m1) m2 = return a `mplus` (interleave m2 m1)
--
-- corollary:
--
-- > interleave m mzero = m
interleave :: (MSplit m, MonadPlus m) => m b -> m b -> m b
interleave sg1 sg2 =
  do r <- msplit sg1
     case r of
       Nothing -> sg2
       Just (sg11,sg12) ->
         (return sg11) `mplus` (interleave sg2 sg12)

-- | Fair (i.e., avoids starvation) conjunction. It obeys the
-- following laws:
--
-- > mzero                >>- k = mzero
-- > (return a `mplus` m) >>- k = interleave (k a) (m >>- k)
(>>-) :: (MonadPlus m, MSplit m) => m a -> (a -> m b) -> m b
sg >>- g =
  do r <- msplit sg
     case r of
       Nothing -> mzero
       Just (sg1 ,sg2) -> interleave (g sg1) (sg2 >>- g)

-- | Collect all solutions. This is from Hinze's @Backtr@ monad
-- class. Unsurprisingly, this can be implemented in terms of msplit.
sols :: (Monad m, MSplit m) => m a -> m [a]
sols m = (msplit m) >>= (fix step) [] where
  step _ jq Nothing          = return jq
  step next jq (Just(a, ma)) = (msplit ma) >>= next (a:jq)

-- | Non-determinism with control (@cut@).
--
-- For the explanation of cut, see Section 5 of Hinze ICFP 2000 paper:
--
-- * [@Backtr@] [Deriving Backtracking Monad Transformers](https://dl.acm.org/citation.cfm?id=351240.351258)
--
-- Hinze suggests expressing @cut@ in terms of @cutfalse@:
--
-- > = return () `mplus` cutfalse
-- > where
-- >  cutfalse :: m a
--
-- satisfies the following laws:
--
-- >  cutfalse >>= k  = cutfalse              (F1)
-- >  cutfalse | m    = cutfalse              (F2)
--
-- (note: @m \``mplus`\` cutfalse@ is different from @cutfalse \``mplus`\` m@).
-- In other words, cutfalse is the left zero of both bind and mplus.
--
-- Hinze also introduces the operation @`call` :: m a -> m a@ that
-- delimits the effect of cut: @`call` m@ executes m. If the cut is
-- invoked in m, it discards only the choices made since m was called.
-- Hinze postulates the axioms of `call`:
--
-- >  call false = false                          (C1)
-- >  call (return a | m) = return a | call m     (C2)
-- >  call (m | cutfalse) = call m                (C3)
-- >  call (lift m >>= k) = lift m >>= (call . k) (C4)
--
-- @`call` m@ behaves like @m@ except any cut inside @m@ has only a local effect,
-- he says.
--
-- Hinze noted a problem with the \"mechanical\" derivation of backtracing
-- monad transformer with cut: no axiom specifying the interaction of
-- call with bind; no way to simplify nested invocations of call.
class Call r where
  -- | Mapping @Backtr@ interface to 'MonadPlus' and using exceptions for
  -- @cutfalse@, every instance should ensure that the following laws hold:
  --
  -- >  cutfalse `mplus` m        = cutfalse                --(F2)
  -- >  call mzero                = mzero                   --(C1)
  -- >  call (return a `mplus` m) = return a `mplus` call m --(C2)
  -- >  call (m `mplus` cutfalse) = call m                  --(C3)
  -- >  call (lift m >>= k)       = lift m >>= (call . k)   --(C4)
  call :: MonadPlus (Eff r) => Eff (Exc CutFalse : r) a -> Eff r a

data CutFalse = CutFalse

-- | We use exceptions for cutfalse
-- Therefore, the law @cutfalse >>= k = cutfalse@
-- is satisfied automatically since all exceptions have the above property.
cutfalse :: Member (Exc CutFalse) r => Eff r a
cutfalse = throwError CutFalse

-- | Prolog @cut@, taken from Hinze 2000 (Deriving backtracking monad
-- transformers).
(!) :: (Member (Exc CutFalse) r, MonadPlus (Eff r)) => Eff r ()
(!) = return () `mplus` cutfalse

-- | Case analysis for lists
{-# INLINE list #-}
list :: b -> (a -> [a] -> b)
     -> [a] -> b
list z _ [] = z
list _ k (h:t) = k h t
