{-# LANGUAGE Safe #-}

-- | Logic primitives. See LogicT paper for details.
module Control.Eff.Logic where

import Control.Monad
import Data.Function (fix)

-- | The MSplit primitive from LogicT paper.
class MSplit m where
  -- | The laws for 'msplit' are:
  --
  -- 1] msplit mzero == return Nothing
  -- 2] msplit (return a `mplus` m) == return (Just(a, m))
  msplit :: m a -> m (Maybe (a, m a))

reflect :: MonadPlus m => Maybe (a, m a) -> m a
reflect Nothing      = mzero
reflect (Just (a,m)) = return a `mplus` m

-- Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper

-- | Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.
--
-- Laws:
-- 1] ifte (return a) th el == th a
-- 2] ifte mzero th el == el
-- 3] ifte (return a `mplus` m) th el == th a `mplus` (m >>= th)
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
-- 1] interleave mzero m == m
-- 2] interleave (return a `mplus` m1) m2 == return a `mplus` (interleave m2 m1)
--
-- corollary:
--   interleave m mzero == m
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
-- 1] mzero >>- k == mzero
-- 2] (return a `mplus` m) >>- k == interleave (k a) (m >>- k)
(>>-) :: (MonadPlus m, MSplit m) => m a -> (a -> m b) -> m b
sg >>- g =
  do r <- msplit sg
     case r of
       Nothing -> mzero
       Just (sg1 ,sg2) -> interleave (g sg1) (sg2 >>- g)

-- | Collect all solutions. This is from Hinze's 'Backtr' monad
-- class. Unsurprisingly, this can be implemented in terms of msplit.
--
-- TODO: use a more efficient data structure.
sols :: (MonadPlus m, MSplit m) => m a -> m [a]
sols m = (msplit m) >>= (fix step) [] where
  step _ jq Nothing = return jq
  step next jq (Just(a, ma)) = (msplit ma) >>= next (a:jq)
