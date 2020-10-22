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
type Arr r a b = a -> Eff r b

-- | An effectful function from @a@ to @b@ that is a composition of one or more
-- effectful functions. The paremeter r describes the overall effect.
--
-- The composition members are accumulated in a type-aligned queue. Using a
-- newtype here enables us to define `C.Category' and `A.Arrow' instances.
newtype Arrs r a b = Arrs (FTCQueue (Eff r) a b)

-- | 'Arrs' can be composed and have a natural identity.
instance C.Category (Arrs r) where
  id = ident
  f . g = comp g f

-- | As the name suggests, 'Arrs' also has an 'A.Arrow' instance.
instance A.Arrow (Arrs r) where
  arr = arr
  first = singleK . first . (^$)

first :: Arr r a b -> Arr r (a, c) (b, c)
first x = \(a,c) -> (, c) `fmap` x a

-- | convert single effectful arrow into composable type. i.e., convert 'Arr' to
-- 'Arrs'
{-# INLINE [2] singleK #-}
singleK :: Arr r a b -> Arrs r a b
singleK k = Arrs (tsingleton k)
{-# RULES
"singleK/qApp" [~2] forall q. singleK (qApp q) = q
 #-}

-- | Application to the `generalized effectful function' @Arrs r b w@, i.e.,
-- convert 'Arrs' to 'Arr'
{-# INLINABLE [2] qApp #-}
qApp :: forall r b w. Arrs r b w -> Arr r b w
qApp (Arrs q) x = viewlMap (inline tviewl q) ($ x) cons
  where
    cons :: forall x. Arr r b x -> FTCQueue (Eff r) x w -> Eff r w
    cons = \k t -> case k x of
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
{-# INLINE [2] (^$) #-}
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
-- In this signature, @r@ is a type-level list of effects that are being
-- requested and need to be handled inside an effectful computation.
-- @a@ is the computation's result similar to other monads.
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

-- | The usual 'bind' function with arguments flipped. This is a
-- common pattern for Eff.
{-# INLINE bind #-}
bind :: Arr r a b -> Eff r a -> Eff r b
bind k e = eff k (E . (^|> k)) e       -- just accumulates continuations

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComp #-}
qComp :: Arrs r a b -> (Eff r b -> k) -> (a -> k)
-- qComp g h = (h . (g `qApp`))
qComp g h = \a -> h (g ^$ a)

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComps #-}
qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = singleK $ qComp g h

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
-- problem". The result is an effectful computation (with possibly some inputs)
-- of effect-type @r@.
class Relay k r where
  relayK :: (v -> k) -> Union r v -> k
  {-# INLINE relay #-}
  relay :: (Eff r' b -> k) -- ^ handler reference
         -> Arrs r' v b -> Union r v -> k
  relay h q u = relayK (qComp q h) u

instance Relay (Eff r w) r where
  {-# INLINABLE relayK #-}
  relayK k u = E (singleK k) u
instance Relay k r => Relay (s -> k) r where
  {-# INLINABLE relayK #-}
  relayK k u s = relayK (\x -> k x s) u

-- | Type synonym for functions written in open-recursive style.
type Open s = s -> s

-- | Respond to requests of type @t@. Usually for each effect @t@, we have to
-- define an instance of the 'Handle' class, which encapsulates how we respond
-- to such requests. This class abstracts a common pattern when implementing
-- effects.
--
-- The 'Handle' class allows us to express the handler logic in an
-- open-recursive (or "shallow") style, i.e., given an explicit handler
-- reference we define how to respond to requests of type @t@. Finally the knot
-- is tied via the application of a fixpoint operator, resulting in a "deep"
-- handler for our effect.
--
-- In other words, we begin by describing how to respond to effect requests via
-- "shallow handlers" of @Kammar, Ohad, Sam Lindley, and Nicolas Oury. "Handlers
-- in action." ACM SIGPLAN Notices. Vol. 48. No. 9. ACM, 2013.@ They are
-- "shallow" because "each handler only handles one step of a computation, in
-- contrast to Plotkin and Pretnar’s deep handlers."  We then convert these into
-- "deep handlers" via the application of a fixpoint operator. Specifically, the
-- "handler reference" argument in 'handle_relay' and 'respond_relay' are fed by
-- tying-the-knot (via the application of 'Data.Function.fix') resulting in a
-- "deep handler". It is this "deep handler" that users of the effect
-- call/invoke.
--
-- The open-recursiveness (of "shallow handlers") awards greater flexibility via
-- delayed knot-tying and makes it easier to write more performant
-- handlers. Specifically, it's fairly straightforward to express handlers which
-- require an intermediate reified data structure. This is possible since the
-- handler result-type @k@ is a type variable and can be of the form @d1 -> k1@
-- where @d1@ is an intermediate data structure. For a concrete example of this,
-- see 'Control.Eff.Logic.NDet.makeChoiceA' (vs the naive and slow
-- 'Control.Eff.Logic.NDet.makeChoiceA0'; and the equally performant, but
-- verbose 'Control.Eff.Logic.NDet.makeChoiceA_manual').
class Handle t r a k where
  -- | Define a single step of computation for the handler, i.e., define how to
  -- "handle" the effectful request.
  --
  -- A handler is a function which accepts an effectful computation as an
  -- argument. A "single step" includes responding to the awaiting coroutine and
  -- passing the result to the handler reference.
  handle :: (Eff r a -> k) -- ^ handler reference
         -> Arrs r v a -- ^ coroutine awaiting response from request
         -> t v -- ^ effect request
         -> k -- ^ the result type after effect is handled

  -- | A convenient pattern: given a request (in an open union), either handle
  -- it (using default Handler) or relay it.
  --
  -- "Handle" implies that all requests of type @t@ are dealt with, i.e., @k@
  -- (the response type) doesn't have @t@ as part of its effect list. The @Relay
  -- k r'@ constraint ensures that @k@ is an effectful computation (with
  -- effect-list @r'@).
  --
  -- Note that we can only handle the leftmost effect type (a consequence of the
  -- 'Data.OpenUnion' implementation).
  handle_relay :: r ~ (t ': r') => Relay k r'
               => (a -> k) -- ^ value handler
               -> Open (Eff r a -> k)
  handle_relay valh self m = eff valh
                            (\q u -> case u of
                                U0 x -> handle self q x
                                U1 u' -> relay self q u')
                            m

  -- | When we only have one effect we don't need to concern ourselves with
  -- relaying.
  handle_terminal :: r ~ '[t]
                  => (a -> k) -- ^ value handler
                  -> Open (Eff r a -> k)
  handle_terminal valh self = eff valh
                              (\q u -> case u of
                                  U0 x -> handle self q x
                                  _ -> error "Impossible: Nothing to relay!")

  -- | Intercept the request and possibly respond to it, but leave it
  -- unhandled. The @Relay k r@ constraint ensures that @k@ is an effectful
  -- computation (with effectlist @r@). As such, the effect type @t@ will show
  -- up in the response type @k@. There are two natural / commmon options for
  -- @k@: the implicit effect domain (i.e., Eff r (f a)), or the explicit effect
  -- domain (i.e., s1 -> s2 -> ... -> sn -> Eff r (f a s1 s2 ... sn)).
  --
  -- There are three different ways in which we may want to alter behaviour:
  --
  -- 1. __Before__: This work should be done before 'respond_relay' is called.
  --
  -- 2. __After__: This work should be done be altering the @ret@ being passed
  -- to 'respond_relay'. This allows us to overwrite changes or discard them
  -- altogether. If this seems magical, note that we have the flexibility of
  -- altering the target domain @k@. Specifically, the explicit domain
  -- representation gives us access to the "effect" realm allowing us to
  -- manipulate it directly.
  -- For an example of 1 and 2, see 'Control.Eff.State.Strict.transactionState'.
  --
  -- 3. __During__: This work should be done by altering the handler being
  -- passed to 'respond_relay'. This allows us to modify the requests "in
  -- flight". For an example, see 'Control.Eff.Writer.Strict.censor'.
  respond_relay :: Member t r => Relay k r
                => (a -> k) -- ^ value handler
                -> Open (Eff r a -> k)
  respond_relay valh self m = eff valh
                             (\q u -> case u of
                                 U0' x -> handle @t self q x
                                 _     -> relay self q u)
                             m

-- so idea is that by composing open-recursive functions you can achieve
-- specializaiton. there is an example with factorial method.

-- | A less commonly needed variant with an explicit handle argument (instead of
-- @Handle t r a k@ constraint).
{-# INLINE handle_relay' #-}
handle_relay' :: r ~ (t ': r') => Relay k r'
              => (forall v. (Eff r a -> k) -> Arrs r v a -> t v -> k) -- ^ effect handler
              -> (a -> k) -- ^ value handler
              -> Open (Eff r a -> k)
handle_relay' hdl valh self m = eff valh
                               (\q u -> case u of
                                   U0 x -> hdl self q x
                                   U1 u' -> relay self q u')
                               m

-- | Handle an effect @t@ via a "lifted" natural transformation to @m@. This is
-- useful when defining handlers which do some kind of @IO@ action. For example,
-- see 'Control.Eff.Trace.runTrace''.
{-# INLINE handle_nat_lifted' #-}
handle_nat_lifted' :: Lifted m r
                   => (forall v. t v -> m v) -- ^ natural transformation
                   -> Open (Eff (t : r) a -> Eff r a)
handle_nat_lifted' f self m = handle_relay' (\h q req -> lift (f req) >>= (qComp q h)) pure self m

-- | Handle an effect @t@ via a natural transformation to @m@. When @m@ is @IO@,
-- 'handle_nat_lifted' might be preferable instead.
{-# INLINE handle_nat' #-}
handle_nat' :: Member m r
            => (forall v. t v -> m v) -- ^ natural transformation
            -> Open (Eff (t : r) a -> Eff r a)
handle_nat' f self m = handle_relay' (\h q req -> send (f req) >>= (qComp q h)) pure self m


-- | Variant with an explicit handle argument (instead of @Handle t r a k@
-- constraint).
{-# INLINE respond_relay' #-}
respond_relay' :: Member t r => Relay k r
               => (forall v. (Eff r a -> k) -> Arrs r v a -> t v -> k) -- ^ effect handler
               -> (a -> k) -- ^ value handler
               -> Open (Eff r a -> k)
respond_relay' hdl valh self m = eff valh
                                (\q u -> case u of
                                    U0' x -> hdl self q x
                                    _     -> relay self q u)
                                m

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

-- |A convenient alias to @SetMember Lift (Lift m) r@, which allows us
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
instance Monad m => Handle (Lift m) r a (m k) where
  handle h q (Lift x) = x >>= (h . (q ^$))

-- | The handler of Lift requests. It is meant to be terminal: we only
-- allow a single Lifted Monad. Note, too, how this is different from
-- other handlers.
runLift :: Monad m => Eff '[Lift m] w -> m w
runLift m = fix (handle_terminal return) m

-- | Catching of dynamic exceptions
-- See the problem in
-- http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO
catchDynE :: forall e a r.
             (Lifted IO r, Exc.Exception e) =>
             Eff r a -> (e -> Eff r a) -> Eff r a
catchDynE m eh = fix (respond_relay' hdl return) m
 where
   -- Polymorphic local binding: signature is needed
   hdl :: (Eff r a -> Eff r a) -> Arrs r v a -> Lift IO v -> Eff r a
   hdl h q (Lift em) = lift (Exc.try em) >>= either eh k
     where k = h . (q ^$)

-- | You need this when using 'catchesDynE'.
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
