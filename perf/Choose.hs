{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -ddump-simpl-stats #-}

import Control.Eff
import Control.Eff.Logic.Choose

xs = [1..100]
ys = choose xs
zs n = fmap (take n) (sols ys) -- not lazy
zs' n = fmap (take n) (sols' ys) -- not lazy
zsNS n = fmap (take n) (makeChoice ys) -- not lazy
zsOnce = once ys -- lazy

runChoice = run . makeChoice
rs n = concat . runChoice $ zs n
rs' n = concat . runChoice $ zs' n
rsNS n = run $ zsNS n
rsOnce = runChoice zsOnce

main = print (rs 5)
