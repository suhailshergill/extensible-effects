{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
module Control.Eff.Logic.Perf (
  result
  -- , rs'
  -- , rsNS
  -- , rsOnce
  ) where

import Control.Eff
import Control.Eff.Logic.NdetEff
import Control.Applicative

xs = [1..100000]
ys = choose xs
-- ys = return 1 <|> return 2 <|> return 3 <|> return 4 <|> return 5 <|>
--      return 6 <|> return 7 <|> return 8 <|> return 9 <|> return 10 <|>
--      empty
--ys = foldr (<|>) empty (map return xs)
-- ys = x1 <|> x2 <|> ... <|> (xn <|> empty)
-- m1 <|> m2 = send MPlus >>= \x -> if x then m1 else m2
-- m1 <|> (m2 <|> m3)
--  = m1 <|> (send MPlus >>= \x -> if x then m2 else m3)
--  = send MPlus >>= \x -> if x then m1
--     else (send MPlus >>= \x -> if x then m2
--      else m3)
zs n = fmap (take n) (sols ys) -- not lazy :: m [a]
--zs' n = fmap (take n) (sols' ys) -- not lazy :: m [a]
--zsNS n = fmap (take n) (makeChoice ys) -- not lazy :: m [a]
-- zsOnce = once ys -- lazy

runChoice = run . makeChoice -- m a -> [a]
rs n = concat . runChoice $ zs n
--rs' n = concat . runChoice $ zs' n
--rsNS n = run $ zsNS n
-- rsOnce = runChoice zsOnce

result = rs 6
