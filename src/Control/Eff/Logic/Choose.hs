{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}
-- This is needed for the MSplit instance
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Nondeterministic choice effect via List interface
module Control.Eff.Logic.Choose ( Choose (..)
                                , sols'
                                , withChoose
                                , choose
                                , makeChoice
                                , module Control.Eff.Logic.Core
                                , msplit1, msplit2
                                ) where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Logic.Core
      
import Control.Applicative as A
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.FCQueue as Q

-- ------------------------------------------------------------------------
-- | Non-determinism (choice)
--
-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
-- Unlike Reader, Choose is not a GADT because the type of values
-- returned in response to a (Choose a) request is just a, without
-- any constraints.
newtype Choose a where
  Choose :: [a] -> Choose a

-- | Embed a pure value
withChoose :: Monad m => a -> m [a]
withChoose = return . (:[])
-- | Given a continuation and a Choose request, respond to it.
instance Monad m => Handle Choose (m [a]) where
  handle _ (Choose []) = return []
  handle k (Choose [x]) = k x
  handle k (Choose lst) = fmap concat $ mapM k lst

instance ( MonadBase m m
         , LiftedBase m r
         ) => MonadBaseControl m (Eff (Choose ': r)) where
    type StM (Eff (Choose ': r)) a = StM (Eff r) [a]
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . makeChoice)
    restoreM x = do lst <- raise (restoreM x)
                    choose lst

sols' :: Member Choose r => Eff r a -> Eff r [a]
sols' = respond_relay' @Choose withChoose
-- sols' (Val x) = return [x]
-- sols' (E q u) = case u of
--   U0' (Choose lst) -> sequence (map (q ^$) lst)
--   _ -> relay (qComp q sols') u

-- | choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
choose :: Member Choose r => [a] -> Eff r a
choose lst = send $ Choose lst

-- | MonadPlus-like operators are expressible via choose
instance Member Choose r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member Choose r => MonadPlus (Eff r) where
  mzero = choose []
  mplus m1 m2 = join $ choose [m1,m2]

-- | Run a nondeterministic effect, returning all values.
makeChoice :: forall a r. Eff (Choose ': r) a -> Eff r [a]
makeChoice = handle_relay withChoose

instance Member Choose r => MSplit (Eff r) where
  msplit = msplit2
  -- msplit (Val x) = withMSplit x mzero
  -- msplit (E q u) = case u of
  --   U0' (Choose lst) -> loop lst
  --   _                -> relay k u
  --   where
  --     k = qComp q msplit
  --     loop []    = return Nothing          -- definite failure
  --     loop [x]   = k x
  --     loop (h:t) = k h >>= \case           -- possibility
  --       Nothing -> loop t                  -- failure, continue exploring
  --       Just (a, m) -> withMSplit a (m `mplus` (loop t >>= reflect)) -- definite success


instance Handle' Choose b (([b] -> k) -> ([b] -> k)) where
  handle' _ (Choose []) dlgt jq = dlgt jq
  -- handle' is now no longer constant time
  handle' k (Choose xs) dlgt jq = dlgt ((map k xs) ++ jq)


instance Handle' Choose b ((FCQueue b -> k) -> (FCQueue b -> k)) where
-- instance Handle' Choose b (b -> (FCQueue b -> k) -> (FCQueue b -> k)) where
-- instance (b ~ Eff r a, Member Choose r) => Handle' Choose b ((FCQueue b -> k) -> (FCQueue b -> k)) where
  handle' _ (Choose []) dlgt jq = dlgt jq
  handle' k (Choose xs) dlgt jq = dlgt $ addOptions .>< jq
    where
      addOptions = foldr (\a q -> q .|> k a) Q.empty xs
      -- z = mzero

msplit1 :: forall r a. Member Choose r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit1 m = loop m [] where
  loop = respond_relay'' ret
         (\k x -> handle' @Choose k x (next' loop))
  ret x jq = withMSplit x (msum jq)

next' :: Alternative f => Monad m
      => (t -> [t] -> m (f a))
      -> [t] -> m (f a)
next' _ [] = return A.empty
next' k (h:t) = k h t


-- NOTE: infinite loop if we use NonEmpty FCQueue
-- TODO: fix still quadratic performance
msplit2 :: forall r a. Member Choose r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit2 m = loop m Q.empty where
  loop = respond_relay'' ret
         (\k x -> handle' @Choose k x (next loop))
  ret :: Member Choose r
      => a
      -> FCQueue (Eff r a)
      -> Eff r (Maybe (a, Eff r a))
  ret x jq = withMSplit x (msum jq)

  next :: Alternative f => Monad m
       => (b -> FCQueue b -> m (f w))
       -> FCQueue b -> m (f w)
  next k q = case viewl q of
    EmptyL -> return A.empty
    h :< t -> k h t
