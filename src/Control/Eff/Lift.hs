{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Safe #-}
-- | Lifting primitive Monad types to effectful computations.
-- We only allow a single Lifted Monad because Monads aren't commutative
-- (e.g. Maybe (IO a) is functionally distinct from IO (Maybe a)).
module Control.Eff.Lift ( Lift (..)
                        , Lifted
                        , LiftedBase
                        , lift
                        , runLift
                        , catchDynE
                        ) where

import Control.Eff.Internal
import qualified Control.Exception as Exc
import Data.OpenUnion

import Control.Monad.Trans.Control (MonadBaseControl)

-- |A convenient alias to 'SetMember Lift (Lift m) r'
type Lifted m r = SetMember Lift (Lift m) r

-- |Same as 'Lifted' but with additional 'MonadBaseControl' constraint
type LiftedBase m r = ( SetMember Lift (Lift m) r
                      , MonadBaseControl m (Eff r)
                      )

-- | Catching of dynamic exceptions
-- See the problem in
-- http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO
catchDynE :: forall e a r.
             (Lifted IO r, Exc.Exception e) =>
             Eff r a -> (e -> Eff r a) -> Eff r a
catchDynE m eh = interpose return h m
 where
   -- Polymorphic local binding: signature is needed
   h :: Arr r v a -> Lift IO v -> Eff r a
   h k (Lift em) = lift (Exc.try em) >>= \x -> case x of
         Right x0 -> k x0
         Left  e -> eh e
