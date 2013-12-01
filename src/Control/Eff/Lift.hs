{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Lifting primitive Monad types to effectful computations.
-- We only allow a single Lifted Monad because Monads aren't commutative
-- (e.g. Maybe (IO a) is functionally distinct from IO (Maybe a)).
module Control.Eff.Lift( Lift
                       , lift
                       , runLift
                       ) where

import Control.Eff
import Data.Typeable

-- | Lift a Monad m to an effect.
data Lift m v = forall a. Lift (m a) (a -> v)

instance Typeable1 m => Typeable1 (Lift m) where
    typeOf1 _ = mkTyConApp (mkTyCon3 "" "Eff" "Lift")
                           [typeOf1 (undefined :: m ())]

instance Functor (Lift m) where
    fmap f (Lift m k) = Lift m (f . k)

instance SetMember Lift (Lift m) (Lift m :> ())

-- | Lift a Monad to an Effect.
lift :: (Typeable1 m, Member (Lift m) r, SetMember Lift (Lift m) r) => m a -> Eff r a
lift m = send (inj . Lift m)

-- | The handler of Lift requests. It is meant to be terminal:
-- we only allow a single Lifted Monad.
runLift :: (Monad m, Typeable1 m) => Eff (Lift m :> ()) w -> m w
runLift m = loop (admin m) where
 loop (Val x) = return x
 loop (E u) = prjForce u $ \(Lift m' k) -> m' >>= loop . k
