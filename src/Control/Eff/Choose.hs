{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe #-}
-- | Nondeterministic choice effect
module Control.Eff.Choose( Choose (..)
                         , choose
                         , runChoice
                         , mzero'
                         , mplus'
                         ) where

import Data.Typeable
import Control.Monad (join)

import Control.Eff

-- | Nondeterministic choice
data Choose v = forall a. Choose [a] (a -> v)
    deriving (Typeable)

instance Functor Choose where
    fmap f (Choose lst k) = Choose lst (f . k)

-- | choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
choose :: Member Choose r => [a] -> Eff r a
choose lst = send . inj $ Choose lst id

-- | MonadPlus-like operators are expressible via choose
mzero' :: Member Choose r => Eff r a
mzero' = choose []

-- | MonadPlus-like operators are expressible via choose
mplus' :: Member Choose r => Eff r a -> Eff r a -> Eff r a
mplus' m1 m2 = join $ choose [m1,m2]

-- | Run a nondeterministic effect, returning all values.
runChoice :: forall a r. Eff (Choose :> r) a -> Eff r [a]
runChoice = loop
 where
  loop = freeMap
         (\x -> return [x])
         (\u -> handleRelay u loop (\(Choose lst k) -> handle lst k))

  handle :: [t] -> (t -> Eff (Choose :> r) a) -> Eff r [a]
  handle [] _  = return []
  handle [x] k = loop (k x)
  handle lst k = concat `fmap` mapM (loop . k) lst
