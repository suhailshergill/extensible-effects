{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

-- The framework of extensible effects

module Control.Eff1 where

import safe Data.OpenUnion51
-- import Data.FastTCQueue
import Data.FTCQueue1
import GHC.Exts (inline)

-- import Data.IORef                       -- For demonstration of lifting
-- import Data.Monoid                      -- For demos

-- ------------------------------------------------------------------------
-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

arr :: (a -> b) -> Arrs r a b
arr f = tsingleton (Val . f)

-- FIXME: convert to 'Arrs'
first :: Arr r a b -> Arr r (a, c) (b, c)
first x = \(a,c) -> (, c) `fmap` x a

ident :: Arrs r a a
ident = tsingleton $ Val . id

comp :: Arrs r a b -> Arrs r b c -> Arrs r a c
comp = (><)

-- | An effectful function from 'a' to 'b' that is a composition
-- of several effectful functions. The paremeter r describes the overall
-- effect.
-- The composition members are accumulated in a type-aligned queue
type Arrs r a b = FTCQueue (Eff r) a b

-- | The Eff monad (not a transformer!)
-- It is a fairly standard coroutine monad
-- It is NOT a Free monad! There are no Functor constraints
-- Status of a coroutine (client): done with the value of type a,
-- or sending a request of type Union r with the continuation
-- Arrs r b a.
-- Potentially, inline Union into E
data Eff r a = Val a
             | forall b. E  (Union r b) (Arrs r b a)

-- Application to the `generalized effectful function' Arrs r b w

{-# INLINABLE qApp #-}
qApp :: Arrs r b w -> b -> Eff r w
qApp q x =
  case inline tviewl q of
    TOne k  -> k x
    k :| t -> case k x of
      Val y -> qApp t y
      E u q0 -> E u (q0 >< t)

{-
-- A bit more understandable version
qApp :: Arrs r b w -> b -> Eff r w
qApp q x = case tviewl q of
   TOne k  -> k x
   k :| t -> bind' (k x) t
 where
   bind' :: Eff r a -> Arrs r a b -> Eff r b
   bind' (Pure y) k     = qApp k y
   bind' (Impure u q) k = Impure u (q >< k)
-}

-- Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComp #-}
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
-- qComp g h = (h . (g `qApp`))
qComp g h = \a -> h $ qApp g a

-- Eff is still a monad and a functor (and Applicative)
-- (despite the lack of the Functor constraint)

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q |> (Val . f)) -- does no mapping yet!

instance Applicative (Eff r) where
  pure = Val
  Val f <*> Val x = Val $ f x
  Val f <*> E u q = E u (q |> (Val . f))
  E u q <*> Val x = E u (q |> (Val . ($ x)))
  E u q <*> m     = E u (q |> (`fmap` m))

instance Monad (Eff r) where
  {-# INLINE return #-}
  {-# INLINE [2] (>>=) #-}
  return x = Val x
  Val x >>= k = k x
  E u q >>= k = E u (q |> k)          -- just accumulates continuations
{-
  Val _ >> m = m
  E u q >> m = E u (q |> const m)
-}

-- send a request and wait for a reply
{-# INLINE [2] send #-}
send :: Member t r => t v -> Eff r v
send t = E (inj t) (tsingleton Val)
-- This seems to be a very beneficial rule! On micro-benchmarks, cuts
-- the needed memory in half and speeds up almost twice.
{-# RULES
  "send/bind" [~3] forall t k. send t >>= k = E (inj t) (tsingleton k)
 #-}


{-
-- The opposite of admin, occasionally useful
-- See the soft-cut for an example
-- It is currently quite inefficient. There are better ways
reflect :: VE a r -> Eff r a
reflect (Val x) = return x
reflect (E u) = Eff (\k -> E $ fmap (loop k) u)
 where
 loop :: (a -> VE w r) -> VE a r -> VE w r
 loop k (Val x) = k x
 loop k (E u)   = E $ fmap (loop k) u
-}


-- ------------------------------------------------------------------------
-- The initial case, no effects

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff '[] w -> w
run (Val x) = x
run (E _ _) = error "extensible-effects: the impossible happened!"
-- the other case is unreachable since Union [] a cannot be
-- constructed.
-- Therefore, run is a total function if its argument terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
{-# INLINE handle_relay #-}
handle_relay :: (a -> Eff r w) ->
                (forall v. t v -> Arr r v w -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handle_relay ret h m = loop m
 where
  loop (Val x)  = ret x
  loop (E u q)  = case decomp u of
    Right x -> h x k
    Left  u0 -> E u0 (tsingleton k)
   where k = qComp q loop

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

-- Intercept the request and possibly reply to it, but leave it unhandled
-- (that's why we use the same r all throuout)
{-# INLINE interpose #-}
interpose :: Member t r =>
             (a -> Eff r w) -> (forall v. t v -> Arr r v w -> Eff r w) ->
             Eff r a -> Eff r w
interpose ret h m = loop m
 where
   loop (Val x)  = ret x
   loop (E u q)  = case prj u of
     Just x -> h x k
     _      -> E u (tsingleton k)
    where k = qComp q loop

-- Parameterized handle_relay
{-# INLINE handle_relay_s #-}
handle_relay_s :: s ->
                (s -> a -> Eff r w) ->
                (forall v. s -> t v -> (s -> Arr r v w) -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handle_relay_s s ret h m = loop s m
  where
    loop s0 (Val x)  = ret s0 x
    loop s0 (E u q)  = case decomp u of
      Right x -> h s0 x k
      Left  u0 -> E u0 (tsingleton (k s0))
     where k s1 x = loop s1 $ qApp q x
