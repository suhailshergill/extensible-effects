{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | Original work available at <http://okmij.org/ftp/Haskell/extensible/Eff.hs>.
-- This module implements extensible effects as an alternative to monad transformers,
-- as described in <http://okmij.org/ftp/Haskell/extensible/exteff.pdf>.
--
-- Extensible Effects are implemented as typeclass constraints on an Eff[ect] datatype.
-- A contrived example can be found under "Control.Eff.Example". To run the
-- effects, consult the tests.
module Control.Eff(
                    Eff
                  , module Reflection
                  , Member
                  , SetMember
                  , Union
                  , (:>)
                  , inj
                  , prj
                  , prjForce
                  , decomp
                  , send
                  , run
                  , interpose
                  , handleRelay
                  , unsafeReUnion
                  ) where

import Control.Monad.Free.Reflection as Reflection
import Data.OpenUnion
import Data.Typeable
import Data.Void

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif

-- | Basic type returned by all computations with extensible effects. The @`Eff`
-- r@ type is a type synonym where the type @r@ is the type of effects that can
-- be handled, and the missing type @a@ (from the type application) is the type
-- of value that is returned.
--
-- Expressed another way: an `Eff` can either be a value (i.e., 'Pure' case), or
-- an effect of type @`Union` r@ producing another `Eff` (i.e., 'Impure'
-- case). The result is that an `Eff` can produce an arbitrarily long chain of
-- @`Union` r@ effects, terminated with a pure value.
--
-- As is made explicit below, the `Eff` type is simply the Free monad resulting from the
-- @`Union` r@ functor.
--
--     @type `Eff` r a = `Free` (`Union` r) a@
type Eff r = Free (Union r)

-- | Given a method of turning requests into results,
-- we produce an effectful computation.
send :: Union r a -> Eff r a
send = freeImpure . (fmap freePure)
{-# INLINE send #-}

-- | Get the result from a pure computation.
run :: Eff Void w -> w
run = freeMap id
      (\_ -> error "extensible-effects: the impossible happened!")
{-# INLINE run #-}
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if m Val terminates.

-- | Given a request, either handle it or relay it.
handleRelay :: Typeable1 t
            => Union (t :> r) v -- ^ Request
            -> (v -> Eff r a)   -- ^ Relay the request
            -> (t v -> Eff r a) -- ^ Handle the request of type t
            -> Eff r a
handleRelay u loop h = either passOn h $ decomp u
  where passOn u' = send u' >>= loop
{-# INLINE handleRelay #-}

-- | Given a request, either handle it or relay it. Both the handler
-- and the relay can produce the same type of request that was handled.
interpose :: (Typeable1 t, Functor t, Member t r)
          => Union r v
          -> (v -> Eff r a)
          -> (t v -> Eff r a)
          -> Eff r a
interpose u loop h = maybe (send u >>= loop) h $ prj u
{-# INLINE interpose #-}
