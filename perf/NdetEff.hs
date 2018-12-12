{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -ddump-simpl-stats #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Control.Monad

import Control.Eff
import Control.Eff.Logic.NdetEff
import Data.FCQueue

{-

- makeChoiceA and makeChoiceA0 almost similar; probably because there aren't any
  failures.

- sols takes 3x memory and time as sols'

-}

makeChoice = makeChoiceLst

xs = [1..100000]
ys = foldl' (\m x -> return x `mplus` m) mzero xs
zs n = fmap (take n) (sols ys) -- not lazy :: m [a]
zs' n = fmap (take n) (sols' ys) -- not lazy :: m [a]
zsNS n = fmap (take n) (makeChoice ys) -- not lazy :: m [a]
zsOnce = once ys -- lazy

runChoice = run . makeChoice -- m a -> [a]
rs n = concat . runChoice $ zs n
rs' n = concat . runChoice $ zs' n
rsNS n = run $ zsNS n
rsOnce = runChoice zsOnce

-- hmm :: FCQueue Int
-- hmm = (singleton 42) .>< undefined

-- bar :: Int
-- bar = case viewl hmm of
--   TOne (UL a) -> a
--   UL h :| _ -> h

main = print (rs 5)
  --print bar
