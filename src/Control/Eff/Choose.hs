{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

-- | Nondeterministic choice effect
module Control.Eff.Choose ( Choose (..)
                          , withChoose
                          , choose
                          , makeChoice
                          , module Control.Eff.Logic
                          ) where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Logic
      
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control

-- ------------------------------------------------------------------------
-- | Non-determinism (choice)
--
-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
-- Unlike Reader, Choose is not a GADT because the type of values
-- returned in response to a (Choose a) request is just a, without
-- any constraints.
newtype Choose a = Choose [a]

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
  msplit = respond_relay (flip withMSplit empty)
           (\k (Choose lst) -> hdl k lst)
    where
      hdl :: Arr r v (Maybe (a, Eff r a))
          -> [v] -> Eff r (Maybe (a, Eff r a))
      hdl _ [] = return Nothing             -- definite failure
      hdl k (h:t) = k h >>= \r -> case r of -- possibility
        Nothing -> hdl k t                  -- failure, continue exploring
        Just (a, m) -> withMSplit a (m <|> (hdl k t >>= reflect)) -- definite success
