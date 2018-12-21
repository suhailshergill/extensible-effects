{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

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
module Control.Eff.Internal where

import qualified Control.Arrow as A
import qualified Control.Category as C
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified Control.Exception as Exc
import safe Data.OpenUnion
import safe Data.FTCQueue
import GHC.Exts (inline)
import Data.Function (fix)

-- | Effectful arrow type: a function from a to b that also does effects
-- denoted by r
newtype Arr r a b = Arr { unArr :: a -> Eff r b }
type ArrT r a b = a -> Eff r b

-- | An effectful function from 'a' to 'b' that is a composition of one or more
-- effectful functions. The paremeter r describes the overall effect.
--
-- The composition members are accumulated in a type-aligned queue.
-- Using a newtype here enables us to define `Category' and `Arrow' instances.
newtype Arrs r a b = Arrs (FTCQueue (Arr r) a b)

-- | 'Arrs' can be composed and have a natural identity.
instance C.Category (Arrs r) where
  id = ident
  f . g = comp g f

-- | As the name suggests, 'Arrs' also has an 'Arrow' instance.
instance A.Arrow (Arrs r) where
  arr = arr
  first = singleK . first . (^$)

first :: ArrT r a b -> ArrT r (a, c) (b, c)
first x = \(a,c) -> (, c) `fmap` x a

-- | convert single effectful arrow into composable type. i.e., convert 'Arr' to
-- 'Arrs'
{-# INLINE singleK #-}
singleK :: ArrT r a b -> Arrs r a b
singleK k = Arrs (tsingleton (Arr k))
{-# INLINE (~^) #-}
(~^) :: ArrT r a b -> Arrs r a b
(~^) k = singleK k

-- | Application to the `generalized effectful function' Arrs r b w, i.e.,
-- convert 'Arrs' to 'Arr'
{-# INLINABLE qApp #-}
qApp :: forall r b w. Arrs r b w -> ArrT r b w
qApp (Arrs q) x = viewlMap (inline tviewl q) (\(Arr f) -> f x) cons
  where
    cons :: forall x. Arr r b x -> FTCQueue (Arr r) x w -> Eff r w
    cons = \(Arr k) t -> case k x of
      Val y -> qApp (Arrs t) y
      E (Arrs q0) u -> E (Arrs (q0 >< t)) u
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
(^$) :: forall r b w. Arrs r b w -> b -> Eff r w
q ^$ x = q `qApp` x

-- | Lift a function to an arrow
arr :: (a -> b) -> Arrs r a b
arr f = singleK (Val . f)

-- | The identity arrow
ident :: Arrs r a a
ident = arr id

-- | Arrow composition
{-# INLINE comp #-}
comp :: Arrs r a b -> Arrs r b c -> Arrs r a c
comp (Arrs f) (Arrs g) = Arrs (f >< g)

-- | Common pattern: append 'Arr' to 'Arrs'
(^|>) :: Arrs r a b -> Arr r b c -> Arrs r a c
(Arrs f) ^|> g = Arrs (f |> g)

-- | The monad that all effects in this library are based on.
--
-- An effectful computation is a value of type `Eff r a`.
-- In this signature, `r` is a type-level list of effects that are being
-- requested and need to be handled inside an effectful computation.
--`a` is the computation's result similar to other monads.
--
-- A computation's result can be retrieved via the 'run' function.
-- However, all effects used in the computation need to be handled by the use
-- of the effects' @run*@ functions before unwrapping the final result.
-- For additional details, see the documentation of the effects you are using.
data Eff r a = Val a
             | forall b. E (Arrs r b a) (Union r b)
-- | Case analysis for 'Eff' datatype. If the value is @'Val' a@ apply
-- the first function to @a@; if it is @'E' u q@, apply the second
-- function.
{-# INLINE eff #-}
eff :: (a -> b)
    -> (forall v. Arrs r v a -> Union r v -> b)
    -> Eff r a -> b
eff f _ (Val a) = f a
eff _ g (E q u) = g q u

-- | The usual 'bind' fnuction with arguments flipped. This is a
-- common pattern for Eff.
{-# INLINE bind #-}
bind :: ArrT r a b -> Eff r a -> Eff r b
bind k e = eff k (E . (^|> Arr k)) e       -- just accumulates continuations

-- | Case analysis for impure computations for 'Eff' datatype. This
-- uses 'decomp'.
{-# INLINE impureDecomp #-}
impureDecomp :: (Arrs (t ': r) v a -> t v -> b)
             -> (Arrs (t ': r) v a -> Union r v -> b)
             -> Arrs (t ': r) v a -> Union (t ': r) v -> b
impureDecomp h rest q u = either (rest q) (h q) (decomp u)
-- | Case analysis for impure computations for 'Eff' datatype. This
-- uses 'prj'.
{-# INLINE impurePrj #-}
impurePrj :: Member t r
          => (Arrs r v a -> t v -> b)
          -> (Arrs r v a -> Union r v -> b)
          -> Arrs r v a -> Union r v -> b
impurePrj h def q u = maybe (def q u) (h q) (prj u)

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComp #-}
qComp :: Arrs r a b -> (Eff r b -> k) -> (a -> k)
-- qComp g h = (h . (g `qApp`))
qComp g h = \a -> h (g ^$ a)
{-# INLINABLE qThen #-}
qThen :: (Eff r b -> k) -> Arrs r a b -> (a -> k)
qThen = flip qComp

-- | Compose and then apply to function. This is a common pattern when
-- processing requests. Different options of 'f' allow us to handle or
-- relay the request and continue on.
{-# INLINE andThen #-}
andThen :: ((b -> c) -> t) -> (Eff r w -> c)
        -> Arrs r b w -> t
andThen f next = f . (qThen next)

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComps #-}
qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = singleK $ qComp g h
{-# INLINABLE (^|$^) #-}
(^|$^) :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
(^|$^) = qComps

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f x = bind (Val . f) x

instance Applicative (Eff r) where
  {-# INLINE pure #-}
  pure = Val
  mf <*> e = bind (`fmap` e) mf

instance Monad (Eff r) where
  {-# INLINE return #-}
  {-# INLINE [2] (>>=) #-}
  return = pure
  m >>= f = bind f m
{-
  Val _ >> m = m
  E q u >> m = E (q ^|> const m) u
-}

-- | Send a request and wait for a reply (resulting in an effectful
-- computation).
{-# INLINE [2] send #-}
send :: Member t r => t v -> Eff r v
send t = E (singleK Val) (inj t)
-- This seems to be a very beneficial rule! On micro-benchmarks, cuts
-- the needed memory in half and speeds up almost twice.
{-# RULES
  "send/bind" [~3] forall t k. send t >>= k = E (singleK k) (inj t)
 #-}


-- ------------------------------------------------------------------------
-- | Get the result from a pure computation
--
-- A pure computation has type @Eff '[] a@. The empty effect-list indicates that
-- no further effects need to be handled.
run :: Eff '[] w -> w
run (Val x) = x
-- | @Union []@ has no nonbottom values.
-- Due to laziness it is possible to get into this branch but its union argument
-- cannot terminate.
-- To extract the true error, the evaluation of union is forced.
-- 'run' is a total function if its argument is different from bottom.
run (E _ union) =
  union `seq` error "extensible-effects: the impossible happened!"

-- | Abstract the recursive 'relay' pattern, i.e., "somebody else's
-- problem".
class Relay k r where
  relay :: (v -> k) -> Union r v -> k
instance Relay (Eff r w) r where
  {-# INLINABLE relay #-}
  relay q u = E (singleK q) u
instance Relay k r => Relay (s -> k) r where
  {-# INLINABLE relay #-}
  relay q u s = relay (\x -> q x s) u

-- | Respond to requests of type 't'.
class Handle t k where
  handle :: (v -> k) -> t v -> k

class Handle' t b k where
  handle' :: (v -> b) -> t v -> k

--newtype Handler t b k = Handler { hdl :: forall v. (v -> b) -> t v -> k }
newtype Handler t r k a = Handler { hdl :: forall v. Arrs r v a -> t v -> k }
newtype Ret k a = Ret { unRet :: a -> k }

{-# INLINE hdl_relay #-}
hdl_relay :: Relay k r
          => Ret k a -> Handler t (t : r) k a
          -> Eff (t : r) a -> k
-- hdl_relay (Ret ret) _ (Val x) = ret x
-- hdl_relay _ (Handler h)
hdl_relay (Ret ret) (Handler h) = loop
  where
    loop (Val x) = ret x
    loop (E q u) = case u of
      U0 x      -> h q x
      U1 u'     -> relay k u'
      where
        k = qComp q loop
{-# INLINE resp_relay #-}
resp_relay :: Relay k r => Member t r
           => Ret k a -> Handler t r k a
           -> Eff r a -> k
resp_relay (Ret ret) (Handler h) = loop
  where
    loop (Val x) = ret x
    loop (E q u) = case u of
      U0' x -> h q x
      _     -> relay k u
      where
        k = qComp q loop


-- | A convenient pattern: given a request (in an open union), either
-- handle it (using default Handler) or relay it.
--
-- "Handle" implies that all requests of type @t@ are dealt with,
-- i.e., @k@ (the response type) doesn't have @t@ as part of its
-- effect list. The @Relay k r@ constraint ensures that @k@ is an
-- effectful computation (with effectlist @r@).
--
-- Note that we can only handle the leftmost effect type (a
-- consequence of the 'OpenUnion' implementation.
{-# INLINE handle_relay #-}
handle_relay :: forall t k r a. Handle t k => Relay k r
             => (a -> k) -- ^ return
             -> Eff (t ': r) a -> k
handle_relay ret = handle_relay' ret handle

-- | A less commonly needed variant with an explicit handler (instead
-- of @Handle t k@ constraint).
{-# INLINE handle_relay' #-}
handle_relay' :: forall t k r a. Relay k r
              => (a -> k) -- ^ return
              -> (forall v. (v -> k) -> t v -> k) -- ^ handler
              -> Eff (t ': r) a -> k
handle_relay' ret h = loop
  where
    loop (Val x) = ret x
    loop (E q u) = case u of
      U0 x      -> h k x
      U1 u'     -> relay k u'
      where
        k = qComp q loop

{-# INLINE handle_relay'' #-}
handle_relay'' :: forall t k r a. Relay k r
              => (a -> k) -- ^ return
              -> (forall v. (v -> Eff (t ': r) a) -> t v -> k) -- ^ handler
              -> Eff (t ': r) a -> k
handle_relay'' ret _ (Val x) = ret x
handle_relay'' ret h (E q u) = case u of
  U0 x      -> h (q ^$) x
  U1 u'     -> relay k u'
    where
      k = qComp q (handle_relay'' ret h)

--{-# INLINE respond_relay'' #-}
respond_relay'' :: Member t r => Relay k r
                => (a -> k)
                -> (forall v. (v -> Eff r a) -> t v -> k)
                -> Eff r a -> k
respond_relay'' ret _ (Val x) = ret x
respond_relay'' ret h (E q u) = case u of
  U0' x -> h (q ^$) x
  _     -> relay k u
    where
      k = qComp q (respond_relay'' ret h)

-- | Intercept the request and possibly respond to it, but leave it
-- unhandled. The @Relay k r@ constraint ensures that @k@ is an effectful
-- computation (with effectlist @r@). As such, the effect type @t@ will show up
-- in the response type @k@. There are two natural / commmon options for @k@:
-- the implicit effect domain (i.e., Eff r (f a)), or the explicit effect domain
-- (i.e., s1 -> s2 -> ... -> sn -> Eff r (f a s1 s2 ... sn)).
--
-- There are three different ways in which we may want to alter behaviour:
-- 1] __Before__: This work should be done before 'respond_relay' is called.
-- 2] __During__: This work should be done by altering the handler being passed
-- to 'respond_relay'. This allows us to modify the requests "in flight".
-- 3] __After__: This work should be done be altering the 'ret' being passed to
-- 'respond_relay'. This allows us to overwrite changes or discard them
-- altogether. If this seems magical, note that we have the flexibility of
-- altering the target domain 'k'. Specifically, the explicit domain
-- representation gives us access to the "effect" realm allowing us to
-- manipulate it directly.
{-# INLINE respond_relay #-}
respond_relay :: Member t r => Relay k r
              => (a -> k)
              -> (forall v. (v -> k) -> t v -> k)
              -> Eff r a -> k
respond_relay ret h = loop
  where
    loop (Val x) = ret x
    loop (E q u) = case u of
      U0' x -> h k x
      _     -> relay k u
      where
        k = qComp q loop

-- | A less common variant which uses the default 'handle' from the @Handle t k@
-- instance (in general, we may need to define new datatypes to call
-- respond_relay with the default handler). Note this variant doesn't allow us
-- to alter effects "during" their execution.
{-# INLINE respond_relay' #-}
respond_relay' :: forall t k r a. (Member t r, Handle t k, Relay k r)
               => (a -> k)
               -> Eff r a -> k
respond_relay' ret = respond_relay ret (handle @t)

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff r a -> Eff (e ': r) a
raise (Val x) = pure x
raise (E q u) = E k (U1 u)
  where k = qComps q raise
{-# INLINE raise #-}

-- ------------------------------------------------------------------------
-- | Lifting: emulating monad transformers
newtype Lift m a = Lift { unLift :: m a }

-- |A convenient alias to 'SetMember Lift (Lift m) r', which allows us
-- to assert that the lifted type occurs ony once in the effect list.
type Lifted m r = SetMember Lift (Lift m) r

-- |Same as 'Lifted' but with additional 'MonadBaseControl' constraint
type LiftedBase m r = ( SetMember Lift (Lift m) r
                      , MonadBaseControl m (Eff r)
                      )

-- | embed an operation of type `m a` into the `Eff` monad when @Lift m@ is in
-- a part of the effect-list.
lift :: Lifted m r => m a -> Eff r a
lift = send . Lift

-- | Handle lifted requests by running them sequentially
instance Monad m => Handle (Lift m) (m k) where
  handle k (Lift x) = x >>= k

-- | The handler of Lift requests. It is meant to be terminal: we only
-- allow a single Lifted Monad. Note, too, how this is different from
-- other handlers.
runLift :: Monad m => Eff '[Lift m] w -> m w
runLift = fix step
  where
    step :: Monad m => (Eff '[Lift m] w -> m w) -> Eff '[Lift m] w -> m w
    step next = eff return
                (impurePrj
                 (handle `andThen` next)
                 (\_ _ -> error "Impossible: Nothing to relay!")
                )

-- | Catching of dynamic exceptions
-- See the problem in
-- http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO
catchDynE :: forall e a r.
             (Lifted IO r, Exc.Exception e) =>
             Eff r a -> (e -> Eff r a) -> Eff r a
catchDynE m eh = respond_relay return h m
 where
   -- Polymorphic local binding: signature is needed
   h :: ArrT r v a -> Lift IO v -> Eff r a
   h k (Lift em) = lift (Exc.try em) >>= either eh k

-- | You need this when using 'catches'.
data HandlerDynE r a =
  forall e. (Exc.Exception e, Lifted IO r) => HandlerDynE (e -> Eff r a)

-- | Catch multiple dynamic exceptions. The implementation follows
-- that in Control.Exception almost exactly. Not yet tested.
-- Could this be useful for control with cut?
catchesDynE :: Lifted IO r => Eff r a -> [HandlerDynE r a] -> Eff r a
catchesDynE m hs = m `catchDynE` catchesHandler hs where
  catchesHandler :: Lifted IO r => [HandlerDynE r a] -> Exc.SomeException -> Eff r a
  catchesHandler handlers e = foldr tryHandler (lift . Exc.throw $ e) handlers
    where
      tryHandler (HandlerDynE h) res = maybe res h (Exc.fromException e)

instance (MonadBase b m, Lifted m r) => MonadBase b (Eff r) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance (MonadBase m m)  => MonadBaseControl m (Eff '[Lift m]) where
    type StM (Eff '[Lift m]) a = a
    liftBaseWith f = lift (f runLift)
    {-# INLINE liftBaseWith #-}
    restoreM = return
    {-# INLINE restoreM #-}

instance (MonadIO m, Lifted m r) => MonadIO (Eff r) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}
