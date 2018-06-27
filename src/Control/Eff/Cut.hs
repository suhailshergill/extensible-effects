{-# LANGUAGE FlexibleContexts, TypeOperators, DataKinds #-}
{-# LANGUAGE Safe #-}
-- | An example of non-trivial interaction of effects, handling of two
-- effects together
-- Non-determinism with control (cut)
-- For the explanation of cut, see Section 5 of Hinze ICFP 2000 paper.
-- Hinze suggests expressing cut in terms of cutfalse:
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
--
-- We use exceptions for cutfalse
-- Therefore, the law @cutfalse >>= k = cutfalse@
-- is satisfied automatically since all exceptions have the above property.
module Control.Eff.Cut where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Exception
import Control.Eff.Choose

data CutFalse = CutFalse

cutfalse :: Member (Exc CutFalse) r => Eff r a
cutfalse = throwError CutFalse

-- | The interpreter -- it is like reify . reflect with a twist.  Compare this
-- implementation with the huge implementation of call in Hinze 2000 (Figure 9).
-- Each clause corresponds to the axiom of call or cutfalse.  All axioms are
-- covered.
--
-- The code clearly expresses the intuition that call watches the choice points
-- of its argument computation. When it encounteres a cutfalse request, it
-- discards the remaining choicepoints.  It completely handles CutFalse effects
-- but not non-determinism
call :: forall a r. Member Choose r => Eff (Exc CutFalse ': r) a -> Eff r a
call m = loop [] m where
  loop :: Member Choose r
       => [Eff (Exc CutFalse ': r) a]
       -> Eff (Exc CutFalse ': r) a
       -> Eff r a
  loop jq (Val x) = return x `mplus'` next jq          -- (C2)
  loop jq (E u q) = case decomp u of
    Right (Exc CutFalse) -> mzero'  -- drop jq (F2)
    Left u0 -> check jq u0 q

  check :: forall b. [Eff (Exc CutFalse ': r) a]
        -> Union r b -> Arrs (Exc CutFalse ': r) b a -> Eff r a
  check jq u _ | Just (Choose []) <- prj u  = next jq  -- (C1)
  check jq u q | Just (Choose [x]) <- prj u = loop jq (q ^$ x)  -- (C3), optim
  check jq u q | Just (Choose lst) <- prj u = next $ map (q ^$) lst ++ jq -- (C3)
  check jq u q = loop jq (E (weaken u) q)     -- (C4)

  next :: Member Choose r
       => [Eff (Exc CutFalse ': r) a]
       -> Eff r a
  next []    = mzero'
  next (h:t) = loop t h
