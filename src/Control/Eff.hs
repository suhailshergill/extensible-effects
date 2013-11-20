{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Original work available at: http://okmij.org/ftp/Hgetell/extensible/Eff.hs.
-- This module implements extensible effects as an alternative to monad transformers,
-- as described in http://okmij.org/ftp/Hgetell/extensible/exteff.pdf.
--
-- Extensible Effects are implemented as typeclass constraints on an Eff[ect] datatype.
-- A contrived example is:
--
--   -- Print a list of numbers, then print their sum.
--   printAndSum :: (Member (Lift IO) e, Member State e) => [Integer] -> Eff e Integer
--   printAndSum (x:xs) = do
--        lift $ putStrLn $ show x
--        onState (+ x)
--   printAndSum [] = getState >>= lift . putStrLn
module Control.Eff( Eff
                  , Member
                  , (:>)
                  , run
                  , send
                  , admin
                  , Reader
                  , runReader
                  , getReader
                  , local
                  , Trace
                  , trace
                  , runTrace
                  , Yield
                  , yield
                  , runC
                  , Y (..)
                  , State
                  , getState
                  , putState
                  , onState
                  , runState
                  , Choose
                  , choose
                  , runChoice
                  , Lift
                  , lift
                  , runLift
                  , Exc
                  , throwError
                  , runError
                  , catchError
                  , Fresh
                  , fresh
                  , runFresh
                  , CutFalse
                  , call
                  , cutfalse
                  ) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Monad (join, ap)
import Data.OpenUnion1
import Data.Typeable

-- | A `VE` is either a value, or an effect of type `Union r` producing another `VE`.
-- The result is that a `VE` can produce an arbitrarily long chain of `Union r`
-- effects, terminated with a pure value.
data VE w r = Val w | E !(Union r (VE w r))

fromVal :: VE w r -> w
fromVal (Val w) = w
fromVal _ = error "fromVal E"

-- | A `Request w r a` consumes values of type `a`, and produces `VE w r`,
-- i.e. a `w` value embedded arbitrarily deep in `Union r` effects.
type Request w r a = a -> VE w r

-- Eff r a can consume a request (i.e. a -> VE w r)
newtype Eff r a = Eff { runEff :: forall w. Request w r a -> VE w r }

instance Functor (Eff r) where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Applicative (Eff r) where
    pure = return
    (<*>) = ap

instance Monad (Eff r) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- send a request and wait for a reply
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff (E . f)

-- administer a client: launch a coroutine and wait for it
-- to send a request or terminate with a value
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

-- ------------------------------------------------------------------------
-- The initial case, no effects

data Void -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run = fromVal . admin
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if m Val terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handleRelay :: Typeable1 t =>
     Union (t :> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handleRelay u loop h = either passOn h $ decomp u
  where passOn u' = send (<$> u') >>= loop
  -- perhaps more efficient:
  -- passOn u' = send (\k -> fmap (\w -> runEff (loop w) k) u')

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

interpose :: (Typeable1 t, Functor t, Member t r) =>
     Union r v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
interpose u loop h = h <$> prj u <?> (send (<$> u) >>= loop)

-- ------------------------------------------------------------------------
-- The Reader monad

-- | The request for a value of type e from the current environment.
-- This environment is analogous to a parameter of type e.
newtype Reader e v = Reader (e -> v)
    deriving (Typeable, Functor)

getReader :: Typeable e => Member (Reader e) r => Eff r e
getReader = send (inj . Reader)

-- | The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w
runReader m e = loop (admin m) where
 loop (Val x) = return x
 loop (E u) = handleRelay u loop (\(Reader k) -> loop (k e))

-- | Locally rebind the value in the dynamic environment.
-- This function both requests and admins Reader requests.
local :: (Typeable e, Member (Reader e) r) =>
     (e -> e) -> Eff r a -> Eff r a
local f m = do
  e <- f <$> getReader
  let loop (Val x) = return x
      loop (E u) = interpose u loop (\(Reader k) -> loop (k e))
  loop (admin m)


-- ------------------------------------------------------------------------
-- Exceptions

-- exceptions of the type e; no resumption
newtype Exc e v = Exc e
    deriving (Functor, Typeable)

-- The type is inferred
throwError :: (Typeable e, Member (Exc e) r) => e -> Eff r a
throwError e = send (\_ -> inj $ Exc e)

runError :: Typeable e => Eff (Exc e :> r) a -> Eff r (Either e a)
runError m = loop (admin m)
 where
 loop (Val x)  = return (Right x)
 loop (E u)    = handleRelay u loop (\(Exc e) -> return (Left e))

-- The handler is allowed to rethrow the exception
catchError :: (Typeable e, Member (Exc e) r) =>
        Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handle = loop (admin m)
 where
 loop (Val x)  = return x
 loop (E u)    = interpose u loop (\(Exc e) -> handle e)


-- ------------------------------------------------------------------------
-- Non-determinism (choice)

-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
data Choose v = forall a. Choose [a] (a -> v)
              deriving (Typeable)

instance Functor Choose where
    fmap f (Choose lst k) = Choose lst (f . k)

choose :: Member Choose r => [a] -> Eff r a
choose lst = send (inj . Choose lst)

-- MonadPlus-like operators are expressible via choose

mzero' :: Member Choose r => Eff r a
mzero' = choose []

mplus' :: Member Choose r => Eff r a -> Eff r a -> Eff r a
mplus' m1 m2 = join $ choose [m1,m2]


-- The interpreter
runChoice :: forall a r. Eff (Choose :> r) a -> Eff r [a]
runChoice m = loop (admin m)
 where
 loop (Val x)  = return [x]
 loop (E u)    = handleRelay u loop (\(Choose lst k) -> handle lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [t] -> (t -> VE a (Choose :> r)) -> Eff r [a]
 handle [] _  = return []
 handle [x] k = loop (k x)
 handle lst k = concat <$> mapM (loop . k) lst


-- ------------------------------------------------------------------------
-- State, strict

data State s w = State (s -> s) (s -> w)
  deriving (Typeable, Functor)

putState :: Typeable e => Member (State e) r => e -> Eff r ()
putState = onState . const

getState :: Typeable e => Member (State e) r => Eff r e
getState = send (inj . State id)

onState :: (Typeable s, Member (State s) r) => (s -> s) -> Eff r ()
onState f = send (\k -> inj (State f (\_ -> k ())))

runState :: Typeable s => s -> Eff (State s :> r) w -> Eff r (w, s)
runState s0 = loop s0 . admin where
 loop s (Val x) = return (x, s)
 loop s (E u)   = handleRelay u (loop s) $
                       \(State t k) -> let s' = t s in s' `seq` loop s' (k s')

-- Examples


-- Encapsulation of effects
-- The example suggested by a reviewer

{- The reviewer outlined an MTL implementation below, writing
  ``This hides the state effect and I can layer another state effect on
  top without getting into conflict with the class system.''

class Monad m => MonadFresh m where
    fresh :: m Int

newtype FreshT m a = FreshT { unFreshT :: State Int m a }
      deriving (Functor, Monad, MonadTrans)

    instance Monad m => MonadFresh (FreshT m) where
      fresh = FreshT $ do n <- get; put (n+1); return n

See EncapsMTL.hs for the complete code.
-}

-- There are three possible implementations
-- The first one uses State Fresh where
--    newtype Fresh = Fresh Int
-- We get the `private' effect layer (State Fresh) that does not interfere
-- with with other layers.
-- This is the easiest implementation.

-- The second implementation defines a new effect Fresh

newtype Fresh i v = Fresh (i -> v)
    deriving (Functor, Typeable)

fresh :: (Typeable i, Enum i, Member (Fresh i) r) => Eff r i
fresh = send (inj . Fresh)

runFresh :: (Typeable i, Enum i) => Eff (Fresh i :> r) w -> i -> Eff r w
runFresh m s0 = loop s0 (admin m)
  where
    loop _ (Val x) = return x
    loop s (E u)   = handleRelay u (loop s) $
                          \(Fresh k) -> (loop $! succ s) (k s)


-- ------------------------------------------------------------------------
-- Tracing (debug printing)

data Trace v = Trace String (() -> v)
    deriving (Typeable, Functor)

-- Printing a string in a trace
trace :: Member Trace r => String -> Eff r ()
trace x = send (inj . Trace x)

-- The handler for IO request: a terminal handler
runTrace :: Eff (Trace :> Void) w -> IO w
runTrace m = loop (admin m) where
 loop (Val x) = return x
 loop (E u)   = prjForce u $ \(Trace s k) -> putStrLn s >> loop (k ())

-- ------------------------------------------------------------------------
-- Lifting: emulating monad transformers

data Lift m v = forall a. Lift (m a) (a -> v)

-- For ST monad, we have to define LiftST since (ST s) can't be Typeable:
-- s must be polymorphic without any constraints

{--
ghci 7.6.3 ==>
Eff.hs:465:29: Warning:
    In the use of `mkTyCon' (imported from Data.Typeable):
    Deprecated: "either derive Typeable, or use mkTyCon3 instead"
--}
instance Typeable1 m => Typeable1 (Lift m) where
    typeOf1 _ =
     mkTyConApp (mkTyCon3 "" "Eff" "Lift") [typeOf1 (undefined:: m ())]

instance Functor (Lift m) where
    fmap f (Lift m k) = Lift m (f . k)

-- | Lift a Monad to an Effect.
lift :: (Typeable1 m, Member (Lift m) r) => m a -> Eff r a
lift m = send (inj . Lift m)

-- | The handler of Lift requests. It is meant to be terminal: we only allow
-- a single Lifted Monad because Monads aren't commutative
-- (e.g. Maybe (IO a) is functionally different from IO (Maybe a)).
runLift :: (Monad m, Typeable1 m) => Eff (Lift m :> Void) w -> m w
runLift m = loop (admin m) where
 loop (Val x) = return x
 loop (E u) = prjForce u $ \(Lift m' k) -> m' >>= loop . k

-- ------------------------------------------------------------------------
-- Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs

-- | The yield request: reporting the value of type e and suspending
-- the coroutine
-- (For simplicity, a co-routine reports a value but accepts unit)
data Yield a v = Yield a (() -> v)
    deriving (Typeable, Functor)

yield :: (Typeable a, Member (Yield a) r) => a -> Eff r ()
yield x = send (inj . Yield x)

-- | Status of a thread: done or reporting the value of the type a
-- (For simplicity, a co-routine reports a value but accepts unit)
data Y r a = Done | Y a (() -> Eff r (Y r a))

-- | Launch a thread and report its status.
runC :: Typeable a => Eff (Yield a :> r) w -> Eff r (Y r a)
runC m = loop (admin m) where
 loop (Val _) = return Done
 loop (E u)   = handleRelay u loop $
                 \(Yield x k) -> return (Y x (loop . k))


-- ------------------------------------------------------------------------
-- An example of non-trivial interaction of effects, handling of two
-- effects together
-- Non-determinism with control (cut)
-- For the explanation of cut, see Section 5 of Hinze ICFP 2000 paper.
-- Hinze suggests expressing cut in terms of cutfalse
--  ! = return () `mplus` cutfalse
-- where
--  cutfalse :: m a
-- satisfies the following laws
--   cutfalse >>= k  = cutfalse              (F1)
--   cutfalse | m    = cutfalse              (F2)
-- (note: m `mplus` cutfalse is different from cutfalse `mplus` m)
-- In other words, cutfalse is the left zero of both bind and mplus.
--
-- Hinze also introduces the operation call :: m a -> m a that
-- delimits the effect of cut: call m executes m. If the cut is
-- invoked in m, it discards only the choices made since m was called.
-- Hinze postulates the axioms of call:
--
--   call false = false                          (C1)
--   call (return a | m) = return a | call m     (C2)
--   call (m | cutfalse) = call m                (C3)
--   call (lift m >>= k) = lift m >>= (call . k) (C4)
--
-- call m behaves like m except any cut inside m has only a local effect,
-- he says.

-- Hinze noted a problem with the `mechanical' derivation of backtracing
-- monad transformer with cut: no axiom specifying the interaction of
-- call with bind; no way to simplify nested invocations of call.

-- We use exceptions for cutfalse
-- Therefore, the law ``cutfalse >>= k       = cutfalse''
-- is satisfied automatically since all exceptions have the above property.

data CutFalse = CutFalse deriving Typeable

cutfalse :: Member (Exc CutFalse) r => Eff r a
cutfalse = throwError CutFalse

-- The interpreter -- it is like reify . reflect with a twist
-- Compare this implementation with the huge implementation of call
-- in Hinze 2000 (Figure 9)
-- Each clause corresponds to the axiom of call or cutfalse.
-- All axioms are covered.
-- The code clearly expresses the intuition that call watches the choice points
-- of its argument computation. When it encounteres a cutfalse request,
-- it discards the remaining choicepoints.

-- It completely handles CutFalse effects but not non-determinism
call :: Member Choose r => Eff (Exc CutFalse :> r) a -> Eff r a
call m = loop [] (admin m) where
 loop jq (Val x) = return x `mplus'` next jq          -- (C2)
 loop jq (E u) = case decomp u of
    Right (Exc CutFalse) -> mzero'  -- drop jq (F2)
    Left u' -> check jq u'

 check jq u | Just (Choose [] _) <- prj u  = next jq  -- (C1)
 check jq u | Just (Choose [x] k) <- prj u = loop jq (k x)  -- (C3), optim
 check jq u | Just (Choose lst k) <- prj u = next $ map k lst ++ jq -- (C3)
 check jq u = send (<$> u) >>= loop jq      -- (C4)

 next []    = mzero'
 next (h:t) = loop t h
