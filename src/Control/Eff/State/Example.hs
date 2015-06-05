module Control.Eff.State.Example where

import Control.Monad.Trans.State.Lazy


-- | example taken from edward kmett's comment here:
-- <http://www.reddit.com/r/haskell/comments/387ex0/are_extensible_effects_a_complete_replacement_for/crt1pzm>
foo:: State [Integer] ()
foo = do
  foo
  modify (1:)

arg:: [Integer]
arg = [1, 2, 3]

bar:: ((), [Integer])
bar = runState foo arg

baz:: ()
baz = fst bar
-- returns ()

qu:: [Integer]
qu = take 3 $ snd bar
-- returns [1, 1, 1]
