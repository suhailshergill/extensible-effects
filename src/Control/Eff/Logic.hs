{-# LANGUAGE Safe #-}

-- | Logic primitives. See LogicT paper for details.
module Control.Eff.Logic where

import Control.Monad

class MSplit m where
  msplit :: m a -> m (Maybe (a, m a))

reflect :: MonadPlus m => Maybe (a, m a) -> m a
reflect Nothing      = mzero
reflect (Just (a,m)) = return a `mplus` m

-- | Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.
ifte :: (MonadPlus m, MSplit m)
     => m t -> (t -> m b) -> m b -> m b
ifte t th el = msplit t >>= check
 where check Nothing          = el
       check (Just (sg1,sg2)) = (th sg1) `mplus` (sg2 >>= th)

-- Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper
once :: (MSplit m, MonadPlus m) => m b -> m b
once m = msplit m >>= check
 where check Nothing        = mzero
       check (Just (sg1,_)) = return sg1

gnot :: (MonadPlus m, MSplit m) => m b -> m ()
gnot m = ifte (once m) (const mzero) (return ())

interleave :: (MSplit m, MonadPlus m) => m b -> m b -> m b
interleave sg1 sg2 =
  do r <- msplit sg1
     case r of
       Nothing -> sg2
       Just (sg11,sg12) ->
         (return sg11) `mplus` (interleave sg2 sg12)

(>>-) :: (MonadPlus m, MSplit m) => m a -> (a -> m b) -> m b
sg >>- g =
  do r <- msplit sg
     case r of
       Nothing -> mzero
       Just (sg1 ,sg2) -> interleave (g sg1) (sg2 >>- g)
