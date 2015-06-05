{-# LANGUAGE TypeOperators #-}
module Control.Eff.State.Example2 where

import Control.Eff
import Control.Eff.State.Lazy

import Data.Tuple (swap)
import Data.Void


-- | example taken from edward kmett's comment here:
-- <http://www.reddit.com/r/haskell/comments/387ex0/are_extensible_effects_a_complete_replacement_for/crt1pzm>
foo:: Eff (State [Integer] :> Void) ()
foo = do
  foo
  modify ((1:: Integer):)

arg:: [Integer]
arg = [1, 2, 3]

ba:: Eff Void ((), [Integer])
ba = swap `fmap` runState arg foo

bar:: ((), [Integer])
bar = run ba

baz:: ()
baz = fst bar
-- loops infinitely, doesn't return

qu:: [Integer]
qu = take 3 $ snd bar
-- loops infinitely, doesn't return
