{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}

-- ------------------------------------------------------------------------
-- | A monadic library for communication between a handler and
-- its client, the administered computation
--
-- Original work available at <http://okmij.org/ftp/Haskell/extensible/tutorial.html>.
-- This module implements extensible effects as an alternative to monad transformers,
-- as described in <http://okmij.org/ftp/Haskell/extensible/exteff.pdf> and
-- <http://okmij.org/ftp/Haskell/extensible/more.pdf>.
--
-- Extensible Effects are implemented as typeclass constraints on an Eff[ect] datatype.
-- A contrived example can be found under "Control.Eff.Example". To run the
-- effects, consult the tests.
module Control.Eff.Internal ( module Control.Eff.Internal
                            ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import qualified Control.Arrow as A
import qualified Control.Category as C
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import safe Data.OpenUnion
import safe Data.FTCQueue
import GHC.Exts (inline)

-- | Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- | An effectful function from 'a' to 'b' that is a composition of one or more
-- effectful functions. The paremeter r describes the overall effect.
--
-- The composition members are accumulated in a type-aligned queue.
-- Using a newtype here enables us to define `Category' and `Arrow' instances.
newtype Arrs r a b = Arrs (FTCQueue (Eff r) a b)

-- | 'Arrs' can be composed and have a natural identity.
instance C.Category (Arrs r) where
  id = ident
  f . g = comp g f

-- | As the name suggests, 'Arrs' also has an 'Arrow' instance.
instance A.Arrow (Arrs r) where
  arr = arr
  first = singleK . first . (^$)

first :: Arr r a b -> Arr r (a, c) (b, c)
first x = \(a,c) -> (, c) `fmap` x a

-- | convert single effectful arrow into composable type. i.e., convert 'Arr' to
-- 'Arrs'
{-# INLINE singleK #-}
singleK :: Arr r a b -> Arrs r a b
singleK = Arrs . tsingleton

-- | Application to the `generalized effectful function' Arrs r b w, i.e.,
-- convert 'Arrs' to 'Arr'
{-# INLINABLE qApp #-}
qApp :: forall r b w. Arrs r b w -> Arr r b w
qApp (Arrs q) x = viewlMap (inline tviewl q) ($ x) cons
  where
    cons :: forall x. Arr r b x -> FTCQueue (Eff r) x w -> Eff r w
    cons = \k t -> case k x of
      Val y -> qApp (Arrs t) y
      E u (Arrs q0) -> E u (Arrs (q0 >< t))
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

-- | Syntactic sugar for 'qApp'
{-# INLINABLE (^$) #-}
(^$) :: forall r b w. Arrs r b w -> Arr r b w
q ^$ x = q `qApp` x

-- | Lift a function to an arrow
arr :: (a -> b) -> Arrs r a b
arr f = singleK (Val . f)

-- | The identity arrow
ident :: Arrs r a a
ident = arr id

-- | Arrow composition
comp :: Arrs r a b -> Arrs r b c -> Arrs r a c
comp (Arrs f) (Arrs g) = Arrs (f >< g)

-- | Common pattern: append 'Arr' to 'Arrs'
(^|>) :: Arrs r a b -> Arr r b c -> Arrs r a c
(Arrs f) ^|> g = Arrs (f |> g)

-- | The Eff monad (not a transformer!). It is a fairly standard coroutine monad
-- where the type @r@ is the type of effects that can be handled, and the
-- missing type @a@ (from the type application) is the type of value that is
-- returned.  It is NOT a Free monad! There are no Functor constraints.
--
-- The two constructors denote the status of a coroutine (client): done with the
-- value of type a, or sending a request of type Union r with the continuation
-- Arrs r b a. Expressed another way: an `Eff` can either be a value (i.e.,
-- 'Val' case), or an effect of type @`Union` r@ producing another `Eff` (i.e.,
-- 'E' case). The result is that an `Eff` can produce an arbitrarily long chain
-- of @`Union` r@ effects, terminated with a pure value.
--
-- Potentially, inline Union into E
data Eff r a = Val a
             | forall b. E  (Union r b) (Arrs r b a)

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComp #-}
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
-- qComp g h = (h . (g `qApp`))
qComp g h = \a -> h $ (g ^$ a)

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComps #-}
qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = singleK $ qComp g h

-- | Eff is still a monad and a functor (and Applicative)
-- (despite the lack of the Functor constraint)
instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q ^|> (Val . f)) -- does no mapping yet!

instance Applicative (Eff r) where
  {-# INLINE pure #-}
  pure = Val
  Val f <*> e = f `fmap` e
  E u q <*> e = E u (q ^|> (`fmap` e))

instance Monad (Eff r) where
  {-# INLINE return #-}
  {-# INLINE [2] (>>=) #-}
  return = pure
  Val x >>= k = k x
  E u q >>= k = E u (q ^|> k)          -- just accumulates continuations
{-
  Val _ >> m = m
  E u q >> m = E u (q ^|> const m)
-}

instance (MonadBase b m, SetMember Lift (Lift m) r) => MonadBase b (Eff r) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance (MonadBase m m)  => MonadBaseControl m (Eff '[Lift m]) where
    type StM (Eff '[Lift m]) a = a
    liftBaseWith f = lift (f runLift)
    {-# INLINE liftBaseWith #-}
    restoreM = return
    {-# INLINE restoreM #-}

instance (MonadIO m, SetMember Lift (Lift m) r) => MonadIO (Eff r) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- | Send a request and wait for a reply (resulting in an effectful
-- computation).
{-# INLINE [2] send #-}
send :: Member t r => t v -> Eff r v
send t = E (inj t) (singleK Val)
-- This seems to be a very beneficial rule! On micro-benchmarks, cuts
-- the needed memory in half and speeds up almost twice.
{-# RULES
  "send/bind" [~3] forall t k. send t >>= k = E (inj t) (singleK k)
 #-}


-- ------------------------------------------------------------------------
-- | The initial case, no effects. Get the result from a pure computation.
--
-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff '[] w -> w
run (Val x) = x
-- | the other case is unreachable since Union [] a cannot be
-- constructed.
-- Therefore, run is a total function if its argument terminates.
run (E _ _) = error "extensible-effects: the impossible happened!"

-- | A convenient pattern: given a request (open union), either
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
    Left  u0 -> E u0 (singleK k)
   where k = qComp q loop

-- | Parameterized handle_relay
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
      Left  u0 -> E u0 (singleK (k s0))
     where k s1 x = loop s1 $ qApp q x

-- | Add something like Control.Exception.catches? It could be useful
-- for control with cut.
--
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
     _      -> E u (singleK k)
    where k = qComp q loop

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff r a -> Eff (e ': r) a
raise = loop
  where
    loop (Val x) = pure x
    loop (E u q) = E (weaken u) $ qComps q loop
{-# INLINE raise #-}

-- ------------------------------------------------------------------------
-- | Lifting: emulating monad transformers
newtype Lift m a = Lift (m a)

-- | We make the Lift layer to be unique, using SetMember
lift :: (SetMember Lift (Lift m) r) => m a -> Eff r a
lift = send . Lift

-- | The handler of Lift requests. It is meant to be terminal:
-- we only allow a single Lifted Monad.
runLift :: Monad m => Eff '[Lift m] w -> m w
runLift (Val x) = return x
runLift (E u q) = case prj u of
                  Just (Lift m) -> m >>= runLift . qApp q
                  Nothing -> error "Impossible: Nothing cannot occur"
