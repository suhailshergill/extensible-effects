{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-orphans #-}
-- | Lifting primitive Monad types to effectful computations.
-- We only allow a single Lifted Monad because Monads aren't commutative
-- (e.g. Maybe (IO a) is functionally distinct from IO (Maybe a)).
module Control.Eff.Lift( Lift (..)
                       , lift
                       , runLift
                       ) where

import Control.Eff
import Control.Monad.Base
import Control.Monad.IO.Class (MonadIO (..))
import Data.Typeable

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#endif

-- | Lift a Monad m to an effect.
data Lift m v = forall a. Lift (m a) (a -> v)
#if MIN_VERSION_base(4,7,0)
    deriving (Typeable) -- starting from ghc-7.8 Typeable can only be derived
#else

instance Typeable1 m => Typeable1 (Lift m) where
    typeOf1 _ = mkTyConApp (mkTyCon3 "" "Eff" "Lift")
                           [typeOf1 (undefined :: m ())]
#endif

instance Functor (Lift m) where
    fmap f (Lift m k) = Lift m (f . k)
    {-# INLINE fmap #-}

instance (Typeable1 m, MonadIO m, SetMember Lift (Lift m) r) => MonadIO (Eff r) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (MonadBase b m, Typeable1 m, SetMember Lift (Lift m) r) => MonadBase b (Eff r) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

-- | Lift a Monad to an Effect.
lift :: (Typeable1 m, SetMember Lift (Lift m) r) => m a -> Eff r a
lift m = send . inj $ Lift m id

-- | The handler of Lift requests. It is meant to be terminal:
-- we only allow a single Lifted Monad.
runLift :: (Monad m, Typeable1 m) => Eff (Lift m :> ()) w -> m w
runLift = loop
  where
    loop = freeMap
           return
           (\u -> prjForce u $ \(Lift m' k) -> m' >>= loop . k)
