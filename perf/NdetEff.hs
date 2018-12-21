{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -ddump-simpl-stats #-}
--{-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE FlexibleContexts #-}

import Control.Eff.Logic.Perf

main = print (result)
