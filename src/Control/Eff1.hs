{-# LANGUAGE RankNTypes #-}
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

import Control.Monad
import Control.Applicative
import Data.OpenUnion51
-- import Data.FastTCQueue
import Data.FTCQueue1
import GHC.Exts (inline)

import Data.IORef                       -- For demonstration of lifting
import Data.Monoid                      -- For demos

-- ------------------------------------------------------------------------
-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- An effectful function from 'a' to 'b' that is a composition
-- of several effectful functions. The paremeter r describes the overall
-- effect.
-- The composition members are accumulated in a type-aligned queue
type Arrs r a b = FTCQueue (Eff r) a b

-- The Eff monad (not a transformer!)
-- It is a fairly standard coroutine monad
-- It is NOT a Free monad! There are no Functor constraints
-- Status of a coroutine (client): done with the value of type w,
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
     E u q -> E u (q >< t)

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
    Left  u -> E u (tsingleton k)
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

-- ------------------------------------------------------------------------
-- The Reader monad

-- The request for a value of type e from the current environment
-- This is a GADT because the type of values
-- returned in response to a (Reader e a) request is not any a;
-- we expect in reply the value of type 'e', the value from the
-- environment. So, the return type is restricted: 'a ~ e'
data Reader e v where
  Reader :: Reader e e

-- One can also define this as
--    data Reader e v = (e ~ v) => Reader
-- and even without GADTs, using explicit coercion:
--    newtype Reader e v = Reader (e->v)
-- In the latter case, when we make the request, we make it as Reader id.
-- So, strictly speaking, GADTs are not really necessary.


-- The signature is inferred
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

-- The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader' :: Eff (Reader e ': r) w -> e -> Eff r w
runReader' m e = loop m where
 loop (Val x) = return x
 loop (E u q) = case decomp u of
                  Right Reader -> loop $ qApp q e
                  Left  u      -> E u (tsingleton (qComp q loop))

-- A different way of writing the above
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handle_relay return (\Reader k -> k e) m

-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  -- Local signature is needed, as always with GADTs
  let h :: Reader e v -> Arr r v a -> Eff r a
      h Reader g = g e
  interpose return h m


-- Examples
add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)


-- The type is inferred
-- t1 :: Member (Reader Int) r => Eff r Int
t1 = ask `add` return (1::Int)

t1' :: Member (Reader Int) r => Eff r Int
t1' = do v <- ask; return (v + 1 :: Int)

-- t1r :: Eff r Int
t1r = runReader t1 (10::Int)

t1rr = 11 == run t1r

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

-- Inferred type
-- t2 :: (Member (Reader Int) r, Member (Reader Float) r) => Eff r Float
t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- t2r :: Member (Reader Float) r => Eff r Float
t2r = runReader t2 (10::Int)
-- t2rr :: Eff r Float
t2rr = flip runReader (20::Float) . flip runReader (10::Int) $ t2

t2rrr = 33.0 == run t2rr

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}
t2rrr' = (33.0 ==) $
         run $ runReader (runReader t2 (20::Float)) (10::Int)

-- The type is inferred
t3 :: Member (Reader Int) r => Eff r Int
t3 = t1 `add` local (+ (10::Int)) t1
t3r = (212 ==) $ run $ runReader t3 (100::Int)


-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftM2 (+) (local (+ (10::Int)) t2)
                (local (+ (30::Float)) t2)

t4rr = (106.0 ==) $ run $ runReader (runReader t4 (10::Int)) (20::Float)

-- The opposite order of layers gives the same result
t4rr' = (106.0 ==) $ run $ runReader (runReader t4 (20::Float)) (10::Int)

addGet :: Member (Reader Int) r => Int -> Eff r Int
addGet x = ask >>= \i -> return (i+x)

addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

-- Map an effectful function
-- The type is inferred
tmap :: Member (Reader Int) r => Eff r [Int]
tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr = ([11,12,13,14,15] ==) $
        run $ runReader tmap (10::Int)

-- ------------------------------------------------------------------------
-- The Writer monad

-- In MTL's Writer monad, the told value must have a |Monoid| type. Our
-- writer has no such constraints. If we write a |Writer|-like
-- interpreter to accumulate the told values in a monoid, it will have
-- the |Monoid o| constraint then

data Writer o x where
  Writer :: o -> Writer o ()

tell :: Member (Writer o) r => o -> Eff r ()
tell o = send $ Writer o


-- rdwr :: (Member (Reader Int) r, Member (Writer String) r)
--  => Eff r Int
rdwr = do
  tell "begin"
  r <- addN 10
  tell "end"
  return r

-- We accumulate the told data in a list, hence no Monoid constraints
-- The code is written to be simple, not optimized.
-- If performance matters, we should convert it to accumulator

runWriter :: Eff (Writer o ': r) a -> Eff r (a,[o])
runWriter = handle_relay (\x -> return (x,[]))
                  (\ (Writer o) k -> k () >>= \ (x,l) -> return (x,o:l))

rdwrr :: (Int,[String])
rdwrr = run . (`runReader` (1::Int)) . runWriter $ rdwr
-- (10,["begin","end"])

-- ------------------------------------------------------------------------
-- Exceptions

-- exceptions of the type e; no resumption
newtype Exc e v = Exc e

-- The type is inferred
throwError :: (Member (Exc e) r) => e -> Eff r a
throwError e = send (Exc e)

runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
runError =
   handle_relay (return . Right) (\ (Exc e) _k -> return (Left e))


-- The handler is allowed to rethrow the exception
catchError :: Member (Exc e) r =>
        Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handle = interpose return (\(Exc e) _k -> handle e) m

-- The type is inferred
et1 :: Eff r Int
et1 = return 1 `add` return 2

et1r = 3 == run et1

-- The type is inferred
et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwError (2::Int)

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Exc Int) Void)
      arising from a use of `et2'
-}

-- The inferred type shows that ex21 is now pure
et21 :: Eff r (Either Int Int)
et21 = runError et2

et21r = Left 2 == run et21


-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)
-- The type is inferred
ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

ex2r = runReader (runErrBig (ex2 ask)) (5::Int)

ex2rr = Right 5 == run ex2r

ex2rr1 = (Left (TooBig 7) ==) $
         run $ runReader (runErrBig (ex2 ask)) (7::Int)

-- Different order of handlers (layers)
ex2rr2 = (Left (TooBig 7) ==) $
         run $ runErrBig (runReader (ex2 ask) (7::Int))

-- Implementing the operator <|> from Alternative:
--  a <|> b does
--   -- tries a, and if succeeds, returns its result
--   -- otherwise, tries b, and if succeeds, returns its result
--   -- otherwise, throws mappend of exceptions of a and b

-- We use MemberU2 in the signature rather than Member to
-- ensure that the computation throws only one type of exceptions.
-- Otherwise, this construction is not very useful.
alttry :: forall e r a. (Monoid e, MemberU2 Exc (Exc e) r) =>
          Eff r a -> Eff r a -> Eff r a
alttry ma mb =
  catchError ma $ \ea ->
  catchError mb $ \eb -> throwError (mappend (ea::e) eb)

-- Test case
t_alttry =
 ([Right 10,Right 10,Right 10,Left "bummer1bummer2"] ==) $
  [
  run . runError $
     (return 1 `add` throwError "bummer1") `alttry`
     (return 10),
  run . runError $
     (return 10) `alttry`
     (return 1 `add` throwError "bummer2"),
  run . runError $
     (return 10) `alttry` return 20,
  run . runError $
     (return 1 `add` throwError "bummer1") `alttry`
     (return 1 `add` throwError "bummer2")
     ]


-- ------------------------------------------------------------------------
-- Non-determinism (choice)

-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
-- Unlike Reader, Choose is not a GADT because the type of values
-- returned in response to a (Choose a) request is just a, without
-- any contraints.
newtype Choose a = Choose [a]

choose :: Member Choose r => [a] -> Eff r a
choose lst = send (Choose lst)

-- -- MonadPlus-like operators are expressible via choose
-- instance Member Choose r => Alternative (Eff r) where
--   empty     = choose []
--   m1 <|> m2 = choose [m1,m2] >>= id

-- instance Member Choose r => MonadPlus (Eff r) where
--   mzero = empty
--   mplus = (<|>)


-- The interpreter
makeChoice :: forall a r. Eff (Choose ': r) a -> Eff r [a]
makeChoice =
  handle_relay (return . (:[])) (\ (Choose lst) k -> handle lst k)
 where
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [t] -> (t -> Eff r [a]) -> Eff r [a]
 handle []  _ = return []
 handle [x] k = k x
 handle lst k = fmap concat $ mapM k lst

exc1 :: Member Choose r => Eff r Int
exc1 = return 1 `add` choose [1,2]

exc11 = makeChoice exc1

exc11r = ([2,3] ==) $ run exc11

-- A different implementation, more directly mapping to MonadPlus
-- interface
data NdetEff a where
  MZero :: NdetEff a
  MPlus :: NdetEff Bool

instance Member NdetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NdetEff r => MonadPlus (Eff r) where
  mzero = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

-- An interpreter
-- The following is very simple, but leaks a lot of memory
-- The cause probably is mapping every failure to empty
-- It takes then a lot of timne and space to store those empty
makeChoiceA0 :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA0 = handle_relay (return . pure) $ \m k -> case m of
    MZero -> return empty
    MPlus -> liftM2 (<|>) (k True) (k False)

-- A different implementation, more involved but faster and taking
-- much less (100 times) less memory.
-- The benefit of the effect framework is that we can have many
-- interpreters.
makeChoiceA :: Alternative f => Eff (NdetEff ': r) a -> Eff r (f a)
makeChoiceA m = loop [] m
 where
   loop [] (Val x)    = return (pure x)
   loop (h:t) (Val x) = loop t h >>= \r -> return (pure x <|> r)
   loop jq (E u q) = case decomp u of
     Right MZero     -> case jq of
       []    -> return empty
       (h:t) -> loop t h
     Right MPlus -> loop (qApp q False : jq) (qApp q True)
     Left  u -> E u (tsingleton (\x -> loop jq (qApp q x)))

testCA = do
  i <- msum . fmap return $ [1..10]
  guard (i `mod` 2 == 0)
  return i

testCA_run :: [Int]
testCA_run = run . makeChoiceA $ testCA


-- ------------------------------------------------------------------------
-- Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.

-- We actually implement LogicT, the non-determinism reflection,
-- of which soft-cut is one instance.
-- See the LogicT paper for an explanation
msplit :: Member NdetEff r => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit = loop []
 where
 -- single result
 loop [] (Val x)  = return (Just (x,mzero))
 -- definite result and perhaps some others
 loop jq (Val x)  = return (Just (x, msum jq))
 -- not yet definite answer
 loop jq (E u q)  = case prj u of
  Just MZero -> case jq of
                   -- no futher choices
                   []     -> return Nothing
                   -- other choices remain, try them
                   (j:jq) -> loop jq j
  Just MPlus -> loop ((qApp q False):jq) (qApp q True)
  _      -> E u (tsingleton k) where k = qComp q (loop jq)

-- Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper
ifte :: Member NdetEff r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = msplit t >>= check
 where check Nothing          = el
       check (Just (sg1,sg2)) = (th sg1) `mplus` (sg2 >>= th)

once :: Member NdetEff r => Eff r a -> Eff r a
once m = msplit m >>= check
 where check Nothing        = mzero
       check (Just (sg1,_)) = return sg1


-- primes (very inefficiently -- but a good example of ifte)
test_ifte = do
  n <- gen
  ifte (do
     d <- gen
     guard $ d < n && n `mod` d == 0
     -- _ <- trace ("d: " ++ show d) (return ())
    )
    (\_->mzero)
    (return n)
 where gen = msum . fmap return $ [2..30]

test_ifte_run :: [Int]
test_ifte_run = run . makeChoiceA $ test_ifte
-- [2,3,5,7,11,13,17,19,23,29]

-- called reflect in the LogicT paper
unmsplit :: Member NdetEff r => (Maybe (a, Eff r a)) -> Eff r a
unmsplit Nothing      = mzero
unmsplit (Just (a,m)) = return a `mplus` m

tsplit =
  (tell "begin" >> return 1) `mplus`
  (tell "end"   >> return 2)

tsplitr10, tsplitr11 :: ([Int],[String])
tsplitr10 = run $ runWriter $ makeChoiceA tsplit
tsplitr11 = run $ runWriter $ makeChoiceA (msplit tsplit >>= unmsplit)


tsplitr20, tsplitr21 :: [(Int,[String])]
tsplitr20 = run $ makeChoiceA $ runWriter tsplit
tsplitr21 = run $ makeChoiceA $ runWriter (msplit tsplit >>= unmsplit)

-- ------------------------------------------------------------------------
-- Combining exceptions and non-determinism

-- Example from the paper

ex2_2 = ([Right 5,Left (TooBig 7),Right 1] ==) $
        run . makeChoice . runErrBig $ ex2 (choose [5,7,1])

-- just like ex1_1 in transf.hs but not at all like ex2_1 in transf.hs

-- with different order of handlers, obtain the desired result of
-- a high-priority exception
ex2_1 = (Left (TooBig 7) ==) $
        run . runErrBig . makeChoice $ ex2 (choose [5,7,1])


-- Errror recovery part
-- The code is the same as in transf1.hs. The inferred signatures differ
-- Was: exRec :: MonadError TooBig m => m Int -> m Int
-- exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
exRec m = catchError m handler
 where handler (TooBig n) | n <= 7 = return n
       handler e = throwError e

ex2r_2 = (Right [5,7,1] ==) $
         run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,1]))
-- Compare with ex2r_1 from transf1.hs

ex2r_2' = ([Right 5,Right 7,Right 1] ==) $
          run . makeChoice . runErrBig $ exRec (ex2 (choose [5,7,1]))
-- Again, all three choices are accounted for.

ex2r_1 = (Left (TooBig 11) ==) $
         run . runErrBig . makeChoice $ exRec (ex2 (choose [5,7,11,1]))
-- Compare with ex2r_2 from transf1.hs

-- ------------------------------------------------------------------------
-- State, strict

{- Initial design:
-- The state request carries with it the state mutator function
-- We can use this request both for mutating and getting the state.
-- But see below for a better design!
data State s v where
  State :: (s->s) -> State s s

In this old design, we have assumed that the dominant operation is
modify. Perhaps this is not wise. Often, the reader is most nominant.
-}
-- See also below, for decomposing the State into Reader and Writer!

-- The conventional design of State
data State s v where
  Get :: State s s
  Put :: !s -> State s ()

-- The signatures are inferred
get :: Member (State s) r => Eff r s
get = send Get
{-# RULES
  "get/bind" forall k. get >>= k = send Get >>= k
 #-}


put :: Member (State s) r => s -> Eff r ()
put s = send (Put s)
{-# RULES
  "put/bind"     forall k v. put v >>= k = send (Put v) >>= k
 #-}
{-# RULES
  "put/semibind" forall k v. put v >>  k = send (Put v) >>= (\() -> k)
 #-}
-- The purpose of the rules is to expose send, which is then being
-- fuzed by the send/bind rule. The send/bind rule is very profitable!
-- These rules are essentially inlining of get/put. Somehow GHC does not
-- inline get/put, even if I put the INLINE directives and play with phases.
-- (Inlining works if I use 'inline' explicitly).

-- Parameterized handle_relay
{-# INLINE handle_relay_s #-}
handle_relay_s :: s ->
                (s -> a -> Eff r w) ->
                (forall v. s -> t v -> (s -> Arr r v w) -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handle_relay_s s ret h m = loop s m
  where
    loop s (Val x)  = ret s x
    loop s (E u q)  = case decomp u of
      Right x -> h s x k
      Left  u -> E u (tsingleton (k s))
     where k s x = loop s $ qApp q x

runState' :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState' m s =
  handle_relay_s s (\s x -> return (x,s))
                   (\s sreq k -> case sreq of
                       Get    -> k s s
                       Put s' -> k s' ())
                   m

-- Since State is so frequently used, we optimize it a bit
runState :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState (Val x) s = return (x,s)
runState (E u q) s = case decomp u of
  Right Get     -> runState (qApp q s) s
  Right (Put s) -> runState (qApp q ()) s
  Left  u -> E u (tsingleton (\x -> runState (qApp q x) s))


-- Examples

ts1 :: Member (State Int) r => Eff r Int
ts1 = do
  put (10 ::Int)
  x <- get
  return (x::Int)

ts1r = ((10,10) ==) $ run (runState ts1 (0::Int))


ts2 :: Member (State Int) r => Eff r Int
ts2 = do
  put (10::Int)
  x <- get
  put (20::Int)
  y <- get
  return (x+y)

ts2r = ((30,20) ==) $ run (runState ts2 (0::Int))


-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
data ProxyState s = ProxyState
transactionState :: forall s r w. Member (State s) r =>
                    ProxyState s -> Eff r w -> Eff r w
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r w -> Eff r w
   loop s (Val x) = put s >> return x
   loop s (E (u::Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (qApp q s)
     Just (Put s') -> loop s'(qApp q ())
     _      -> E u (tsingleton k) where k = qComp q (loop s)

-- A different representation of State: decomposing State into
-- mutation (Writer) and Reading. We don't define any new effects:
-- we just handle the existing ones.
-- Thus we define a handler for two effects together.

runStateR :: Eff (Writer s ': Reader s ': r) w -> s -> Eff r (w,s)
runStateR m s = loop s m
 where
   loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w,s)
   loop s (Val x) = return (x,s)
   loop s (E u q) = case decomp u of
     Right (Writer o) -> k o ()
     Left  u  -> case decomp u of
       Right Reader -> k s s
       Left u -> E u (tsingleton (k s))
    where k s = qComp q (loop s)

-- If we had a Writer, we could have decomposed State into Writer and Reader
-- requests.

ts11 :: (Member (Reader Int) r, Member (Writer Int) r) => Eff r Int
ts11 = do
  tell (10 ::Int)
  x <- ask
  return (x::Int)

ts11r = ((10,10) ==) $ run (runStateR ts11 (0::Int))


ts21 :: (Member (Reader Int) r, Member (Writer Int) r) => Eff r Int
ts21 = do
  tell (10::Int)
  x <- ask
  tell (20::Int)
  y <- ask
  return (x+y)

ts21r = ((30,20) ==) $ run (runStateR ts21 (0::Int))


-- exceptions and state
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1::Int))

tes1 :: (Member (State Int) r, Member (Exc [Char]) r) => Eff r b
tes1 = do
 incr
 throwError "exc"

ter1 = ((Left "exc" :: Either String Int,2) ==) $
       run $ runState (runError tes1) (1::Int)


ter2 = ((Left "exc" :: Either String (Int,Int)) ==) $
       run $ runError (runState tes1 (1::Int))


teCatch :: Member (Exc String) r => Eff r a -> Eff r [Char]
teCatch m = catchError (m >> return "done") (\e -> return (e::String))

ter3 = ((Right "exc" :: Either String String,2) ==) $
       run $ runState (runError (teCatch tes1)) (1::Int)

ter4 = ((Right ("exc",2) :: Either String (String,Int)) ==) $
       run $ runError (runState (teCatch tes1) (1::Int))


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

data Fresh v where
  Fresh :: Fresh Int

fresh :: Member Fresh r => Eff r Int
fresh = send Fresh

-- And a handler for it
runFresh' :: Eff (Fresh ': r) w -> Int -> Eff r w
runFresh' m s =
  handle_relay_s s (\_s x -> return x)
                   (\s Fresh k -> (k $! s+1) s)
                   m

-- Test
tfresh' = runTrace $ flip runFresh' 0 $ do
  n <- fresh
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n
{-
Fresh 0
Fresh 1
-}

{-
-- Finally, the worst implementation but the one that answers
-- reviewer's question: implementing Fresh in terms of State
-- but not revealing that fact.

runFresh :: Eff (Fresh :> r) w -> Int -> Eff r w
runFresh m s = runState m' s >>= return . fst
 where
 m' = loop m
 loop (Val x) = return x
 loop (E u q)   = case decomp u of
  Right Fresh -> do
                 n <- get
                 put (n+1::Int)
                 k n
  Left u  -> send (\k -> weaken $ fmap k u) >>= loop

tfresh = runTrace $ flip runFresh 0 $ do
  n <- fresh
  -- (x::Int) <- get
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n

{-
If we try to meddle with the encapsulated state, by uncommenting the
get statement above, we get:
    No instance for (Member (State Int) Void)
      arising from a use of `get'
-}

-}

-- ------------------------------------------------------------------------
-- Tracing (debug printing)

data Trace v where
  Trace :: String -> Trace ()

-- Printing a string in a trace
trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

-- The handler for IO request: a terminal handler
runTrace :: Eff '[Trace] w -> IO w
runTrace (Val x) = return x
runTrace (E u q) = case decomp u of
     Right (Trace s) -> putStrLn s >> runTrace (qApp q ())
     -- Nothing more can occur

-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, Member Trace r) =>
     (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
 trace $ "mapMdebug: " ++ show h
 h' <- f h
 t' <- mapMdebug f t
 return (h':t')

tMd = runTrace $ runReader (mapMdebug f [1..5]) (10::Int)
 where f x = ask `add` return x
{-
mapMdebug: 1
mapMdebug: 2
mapMdebug: 3
mapMdebug: 4
mapMdebug: 5
[11,12,13,14,15]
-}

-- duplicate layers
tdup = runTrace $ runReader m (10::Int)
 where
 m = do
     runReader tr (20::Int)
     tr
 tr = do
      v <- ask
      trace $ "Asked: " ++ show (v::Int)
{-
Asked: 20
Asked: 10
-}

-- ------------------------------------------------------------------------
-- Lifting: emulating monad transformers

newtype Lift m a = Lift (m a)

-- We make the Lift layer to be unique, using MemberU2
lift :: (MemberU2 Lift (Lift m) r) => m a -> Eff r a
lift = send . Lift

-- The handler of Lift requests. It is meant to be terminal
runLift :: Monad m => Eff '[Lift m] w -> m w
runLift (Val x) = return x
runLift (E u q) = case prj u of
                  Just (Lift m) -> m >>= runLift . qApp q
                  -- Nothing cannot occur

tl1 = ask >>= \(x::Int) -> lift . print $ x

-- tl1r :: IO ()
tl1r = runLift (runReader tl1 (5::Int))
-- 5

-- Re-implemenation of mapMdebug using Lifting
-- The signature is inferred
mapMdebug'  :: (Show a, MemberU2 Lift (Lift IO) r) =>
             (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug' f [] = return []
mapMdebug' f (h:t) = do
 lift $ print h
 h' <- f h
 t' <- mapMdebug' f t
 return (h':t')

tMd' = runLift $ runReader (mapMdebug' f [1..5]) (10::Int)
 where f x = ask `add` return x
{-
1
2
3
4
5
[11,12,13,14,15]
-}

-- Example by Oscar Key
data Move x where
  Move :: Move ()

handUp :: Eff (Move ': r) a -> Eff r a
handUp (Val x) = return x
handUp (E u q) = case decomp u of
  Right Move -> handDown $ qApp q ()
  -- Relay other requests
  Left u     -> E u (tsingleton Val) >>= handUp . qApp q

handDown :: Eff (Move ': r) a -> Eff r a
handDown (Val x) = return x
handDown (E u q) = case decomp u of
  Right Move -> handUp $ qApp q ()
  -- Relay other requests
  Left u     -> E u (tsingleton Val) >>= handDown . qApp q

{-
 -- ------------------------------------------------------------------------
-- Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs

-- The yield request: reporting the value of type a and suspending
-- the coroutine. Resuming with the value of type b
data Yield a b v = Yield a (b -> v)
    deriving (Typeable, Functor)

-- The signature is inferred
yield :: (Typeable a, Typeable b, Member (Yield a b) r) => a -> Eff r b
yield x = send (inj . Yield x)

-- Status of a thread: done or reporting the value of the type a
-- and resuming with the value of type b
data Y r a b = Done | Y a (b -> Eff r (Y r a b))

-- Launch a thread and report its status
runC :: (Typeable a, Typeable b) =>
        Eff (Yield a b :> r) w -> Eff r (Y r a b)
runC m = loop (admin m) where
 loop (Val x) = return Done
 loop (E u)   = handle_relay u loop $
                 \(Yield x k) -> return (Y x (loop . k))


-- First example of coroutines
yieldInt :: Member (Yield Int ()) r => Int -> Eff r ()
yieldInt = yield

th1 :: Member (Yield Int ()) r => Eff r ()
th1 = yieldInt 1 >> yieldInt 2


c1 = runTrace (loop =<< runC th1)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
1
2
Done
-}

-- Add dynamic variables
-- The code is essentially the same as that in transf.hs (only added
-- a type specializtion on yield). The inferred signature is different though.
-- Before it was
--    th2 :: MonadReader Int m => CoT Int m ()
-- Now it is more general:
th2 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th2 = ask >>= yieldInt >> (ask >>= yieldInt)


-- Code is essentially the same as in transf.hs; no liftIO though
c2 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
Done
-}

-- locally changing the dynamic environment for the suspension
c21 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
Done
-}

-- Real example, with two sorts of local rebinding
th3 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
 where ay = ask >>= yieldInt

c3 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
20
20
Done
-}

-- locally changing the dynamic environment for the suspension
c31 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
21
21
Done
-}
-- The result is exactly as expected and desired: the coroutine shares the
-- dynamic environment with its parent; however, when the environment
-- is locally rebound, it becomes private to coroutine.

-- We now make explicit that the client computation, run by th4,
-- is abstract. We abstract it out of th4
c4 = runTrace $ runReader (loop =<< runC (th4 client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       th4 cl = cl >> local (+(10::Int)) cl
       client = ay >> ay
       ay     = ask >>= yieldInt

{-
10
11
21
21
Done
-}

-- Even more dynamic example
c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (\y->x+1) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= yieldInt

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
10
11
12
18
18
18
29
29
29
29
29
29
Done
-}

-- And even more
c7 = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)

{-
1010
1021
1032
1048
1064
1080
1101
1122
1143
1169
1195
1221
1252
1283
1314
1345
1376
1407
Done
-}

c7' = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Double))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
1010
1021
1032
1048
1048
1048
1069
1090
1111
1137
1137
1137
1168
1199
1230
1261
1292
1323
Done
-}

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
    Left u -> check jq u

 check jq u | Just (Choose [] _) <- prj u  = next jq  -- (C1)
 check jq u | Just (Choose [x] k) <- prj u = loop jq (k x)  -- (C3), optim
 check jq u | Just (Choose lst k) <- prj u = next $ map k lst ++ jq -- (C3)
 check jq u = send (\k -> fmap k u) >>= loop jq      -- (C4)

 next []    = mzero'
 next (h:t) = loop t h

-- The signature is inferred
tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
tcut1 = (return (1::Int) `mplus'` return 2) `mplus'`
         ((cutfalse `mplus'` return 4) `mplus'`
          return 5)

tcut1r = run . makeChoice $ call tcut1
-- [1,2]

tcut2 = return (1::Int) `mplus'`
         call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'`
               return 4)
       `mplus'` return 5

-- Here we see nested call. It poses no problems...
tcut2r = run . makeChoice $ call tcut2
-- [1,2,5]

-- More nested calls
tcut3 = call tcut1 `mplus'` call (tcut2 `mplus'` cutfalse)
tcut3r = run . makeChoice $ call tcut3
-- [1,2,1,2,5]

tcut4 = call tcut1 `mplus'`  (tcut2 `mplus'` cutfalse)
tcut4r = run . makeChoice $ call tcut4
-- [1,2,1,2,5]
-}
