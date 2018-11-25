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
module Control.Eff.Internal where

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
import Data.Function (fix)

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
{-# INLINE (~^) #-}
(~^) :: Arr r a b -> Arrs r a b
(~^) = singleK

-- | Application to the `generalized effectful function' Arrs r b w, i.e.,
-- convert 'Arrs' to 'Arr'
{-# INLINABLE qApp #-}
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
bind :: Arr r a b -> Eff r a -> Eff r b
bind k = eff k (E . (^|> k))         -- just accumulates continuations

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
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
-- qComp g h = (h . (g `qApp`))
qComp g h = \a -> h $ (g ^$ a)
{-# INLINABLE qThen #-}
qThen :: (Eff r b -> Eff r' c) -> Arrs r a b -> Arr r' a c
qThen = flip qComp

-- | Compose effectful arrows (and possibly change the effect!)
{-# INLINE qComps #-}
qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = singleK $ qComp g h
{-# INLINABLE (^|$^) #-}
(^|$^) :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
(^|$^) = qComps

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f = bind (Val . f)

instance Applicative (Eff r) where
  {-# INLINE pure #-}
  pure = Val
  mf <*> e = bind (`fmap` e) mf

instance Monad (Eff r) where
  {-# INLINE return #-}
  {-# INLINE [2] (>>=) #-}
  return = pure
  (>>=) = flip bind
{-
  Val _ >> m = m
  E q u >> m = E (q ^|> const m) u
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

-- | Higher-order 'on' function
on :: ((t2 -> x1) -> (t2 -> x2) -> t3)
   -> (forall x. (t1 -> x) -> t2 -> x)
   -> (t1 -> x1)
   -> (t1 -> x2)
   -> t3
on f w x y = f (w x) (w y)

-- | A convenient pattern: given a request (open union), either
-- handle it or relay it.
{-# INLINE handle_relay #-}
handle_relay :: (a -> Eff r w) ->
                (forall v. Arr r v w -> t v -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handle_relay ret h = fix step                 -- limit
 where
  step next = eff ret                           -- base
              (on impureDecomp (. (qThen next)) -- recurse
                h                                 -- handle
                (E . (~^))                        -- relay
              )

-- | Parameterized handle_relay
{-# INLINE handle_relay_s #-}
handle_relay_s :: (s -> a -> Eff r w)
               -> (forall v. s -> (s -> Arr r v w) -> t v -> Eff r w)
               -> s -> Eff (t ': r) a -> Eff r w
handle_relay_s ret h = fix step                            -- limit
  where
    step next s = eff (ret s)                                -- base
                  (on impureDecomp (. (flip (qThen . next))) -- recurse
                    (h $ s)                                    -- handle
                    (E . (~^) . ($ s))                         -- relay
                  )

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

-- | Intercept the request and possibly respond to it, but leave it
-- unhandled (that's why the same r is used all throughout).
{-# INLINE interpose #-}
interpose :: Member t r
          => (a -> Eff r w)
          -> (forall v. Arr r v w -> t v -> Eff r w)
          -> Eff r a -> Eff r w
interpose ret h = fix step                  -- limit
 where
   step next = eff ret                        -- base
               (on impurePrj (. (qThen next)) -- recurse
                 h                              -- respond
                 (E . (~^))                     -- relay
               )

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff r a -> Eff (e ': r) a
raise = fix step
  where
    step next = eff pure
                (\q -> ((E . (~^) . (qThen next)) q) . weaken)
{-# INLINE raise #-}

-- ------------------------------------------------------------------------
-- | Lifting: emulating monad transformers
newtype Lift m a = Lift { unLift :: m a }

-- | embed an operation of type `m a` into the `Eff` monad when @Lift m@ is in
-- a part of the effect-list.
--
-- By using SetMember, it is possible to assert that the lifted type occurs
-- only once in the effect list
lift :: (SetMember Lift (Lift m) r) => m a -> Eff r a
lift = send . Lift

-- | The handler of Lift requests. It is meant to be terminal:
-- we only allow a single Lifted Monad.
runLift :: Monad m => Eff '[Lift m] w -> m w
runLift = eff return
          (impurePrj
            (\q x -> (unLift x) >>= (runLift . (q ^$)))
            (\_ _ -> error "Impossible: Nothing cannot occur")
          )
