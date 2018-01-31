{-# LANGUAGE Safe #-}
-- | Lifting primitive Monad types to effectful computations.
-- We only allow a single Lifted Monad because Monads aren't commutative
-- (e.g. Maybe (IO a) is functionally distinct from IO (Maybe a)).
module Control.Eff.Lift ( Lift (..)
                        , lift
                        , runLift
                        , catchDynE
                        ) where

import Control.Eff.Internal
import qualified Control.Exception as Exc
import Data.OpenUnion

-- | Catching of dynamic exceptions
-- See the problem in
-- http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO
catchDynE :: forall e a r.
             (SetMember Lift (Lift IO) r, Exc.Exception e) =>
             Eff r a -> (e -> Eff r a) -> Eff r a
catchDynE m eh = interpose return h m
 where
   -- Polymorphic local binding: signature is needed
   h :: Lift IO v -> Arr r v a -> Eff r a
   h (Lift em) k = lift (Exc.try em) >>= \x -> case x of
         Right x0 -> k x0
         Left  e -> eh e
