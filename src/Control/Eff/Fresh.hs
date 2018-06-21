{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
-- | Create unique Enumerable values.
module Control.Eff.Fresh( Fresh (Fresh)
                        , fresh
                        , runFresh'
                        ) where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Lift

import Control.Monad.Base
import Control.Monad.Trans.Control


-- There are three possible implementations
-- The first one uses State Fresh where
--    newtype Fresh = Fresh Int
-- We get the `private' effect layer (State Fresh) that does not interfere
-- with with other layers.
-- This is the easiest implementation.

-- The second implementation defines a new effect Fresh

-- | Create unique Enumerable values.
data Fresh v where
  Fresh :: Fresh Int
  Replace :: !Int -> Fresh ()

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
         ) => MonadBaseControl m (Eff (Fresh ': r)) where
    type StM (Eff (Fresh ': r)) a = StM (Eff r) (a, Int)
    liftBaseWith f = do i <- fresh
                        raise $ liftBaseWith $ \runInBase ->
                          f (\k -> runInBase $ runFreshReturn k i)
    restoreM x = do (r,i) <- raise (restoreM x)
                    replace i
                    return r


-- | Produce a value that has not been previously produced.
fresh :: Member Fresh r => Eff r Int
fresh = send Fresh

replace :: Member Fresh r => Int -> Eff r ()
replace = send . Replace

-- | Run an effect requiring unique values.
runFresh' :: Eff (Fresh ': r) w -> Int -> Eff r w
runFresh' m s = fst `fmap` runFreshReturn m s

runFreshReturn :: Eff (Fresh ': r) w -> Int -> Eff r (w,Int)
runFreshReturn m s =
  handle_relay_s s (\s' x -> return (x,s'))
                   (\s' e k -> case e of
                                 Fresh -> (k $! s' + 1) s'
                                 Replace i -> k i ())
                   m
{-
-- Finally, the worst implementation but the one that answers
-- reviewer's question: implementing Fresh in terms of State
-- but not revealing that fact.

runFresh :: Eff (Fresh :> r) w -> Int -> Eff r w
runFresh m s = runState m' s >>= return . fst
 where
 m' = loop m
 loop (Val x) = return x
 loop (E u q)   = case decomp u of
  Right Fresh -> do
                 n <- get
                 put (n+1::Int)
                 k n
  Left u  -> send (\k -> weaken $ fmap k u) >>= loop

tfresh = runTrace $ flip runFresh 0 $ do
  n <- fresh
  -- (x::Int) <- get
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n

{-
If we try to meddle with the encapsulated state, by uncommenting the
get statement above, we get:
    No instance for (Member (State Int) Void)
      arising from a use of `get'
-}

-}

-- Encapsulation of effects
-- The example suggested by a reviewer

{- The reviewer outlined an MTL implementation below, writing
  ``This hides the state effect and I can layer another state effect on
  top without getting into conflict with the class system.''

class Monad m => MonadFresh m where
    fresh :: m Int

newtype FreshT m a = FreshT { unFreshT :: State Int m a }
      deriving (Functor, Monad, MonadTrans)

    instance Monad m => MonadFresh (FreshT m) where
      fresh = FreshT $ do n <- get; put (n+1); return n

See EncapsMTL.hs for the complete code.
-}
