--{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

-- | Nondeterministic choice effect
module Control.Eff.Choose ( Choose (..)
                          , choose
                          , makeChoice
                          , mzero'
                          , mplus'
                          ) where

import Control.Eff
import Data.OpenUnion

-- ------------------------------------------------------------------------
-- | Non-determinism (choice)
--
-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
-- Unlike Reader, Choose is not a GADT because the type of values
-- returned in response to a (Choose a) request is just a, without
-- any constraints.
newtype Choose a = Choose [a]

-- | choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
choose :: Member Choose r => [a] -> Eff r a
choose lst = send $ Choose lst

-- | MonadPlus-like operators are expressible via choose
mzero' :: Member Choose r => Eff r a
mzero' = choose []

-- | MonadPlus-like operators are expressible via choose
mplus' :: Member Choose r => Eff r a -> Eff r a -> Eff r a
mplus' m1 m2 = choose [m1,m2] >>= id

-- FIXME: find a way to uncomment
-- -- MonadPlus-like operators are expressible via choose
-- instance Member Choose r => Alternative (Eff r) where
--   empty     = choose []
--   m1 <|> m2 = choose [m1,m2] >>= id

-- instance Member Choose r => MonadPlus (Eff r) where
--   mzero = empty
--   mplus = (<|>)

-- | Run a nondeterministic effect, returning all values.
makeChoice :: forall a r. Eff (Choose ': r) a -> Eff r [a]
makeChoice = handle_relay
  (return . (:[]))
  (\(Choose lst) k -> handle lst k)
  where
    handle :: [t] -> (t -> Eff r [a]) -> Eff r [a]
    handle []  _ = return []
    handle [x] k = k x
    handle lst k = fmap concat $ mapM k lst
