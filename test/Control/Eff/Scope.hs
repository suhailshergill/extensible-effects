{-# OPTIONS_GHC -Wmissing-signatures #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Control.Eff.Scope where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Extend hiding (eff)
import qualified Control.Eff.Extend as E
import Control.Eff.Exception
import Control.Eff.Writer.Strict
import Control.Eff.State.Strict

import Control.Monad
import Control.Eff.Logic.Core
import Control.Eff.Logic.NDet

import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

decr :: '[State Integer, Fail] <:: r => Eff r ()
decr = do
  x <- get
  if x > 0 then put (x - 1) else die
incr :: Member (State Integer) r => Eff r ()
incr = get >>= \x -> put (x + 1)

tripleDecr, tripleDecrTx, tripleDecrLocal, tripleDecrIncrLocal :: '[State Integer, Fail] <:: r => Eff r ()
tripleDecr = prog catchError decr return
tripleDecrTx = prog catchError (mkTx decr) return
tripleDecrLocal = prog (catchError . mkTx) decr return
tripleDecrIncrLocal = prog ((catchError @()) . mkTx) decr (\_ -> incr)

mkTx = transactionState (TxState :: TxState Integer)
prog :: Monad m => (m b1 -> t -> m b2) -> m b1 -> t -> m b2
prog catch d r = d >> catch (d >> d) r
prog2 :: Monad (repr r) => Catch' e repr r a
      => repr r a -> (e -> repr r a) -> repr r a
prog2 d r = d >> catch' (d >> d) r

tripleDecr_eval1 :: Eff '[State Integer, Fail] () -> Either () ((), Integer)
tripleDecr_eval1 = run . runError . (runState (2::Integer))
tripleDecr_eval2 :: Eff '[Fail, State Integer] () -> (Either () (), Integer)
tripleDecr_eval2 = run . (runState (2::Integer)) . runError

case_scope_global =
  (Right ((), (0::Integer)) @=? tripleDecr_eval1 tripleDecr)
  >> (Right ((), (0::Integer)) @=? tripleDecr_eval1 (mkTx tripleDecr))
  >> (Right ((), (0::Integer)) @=? tripleDecr_eval1 tripleDecrTx)
  >> ((Right (), (0::Integer)) @=? tripleDecr_eval2 tripleDecr)
  >> ((Right (), (0::Integer)) @=? tripleDecr_eval2 (mkTx tripleDecr))
  >> ((Right (), (0::Integer)) @=? tripleDecr_eval2 tripleDecrTx)

-- Lesson: For algebraic operations, order of handlers alters semantics. For
-- non-algebraic operations, semantics are fixed by the handlers (and not their
-- specific order). We see how 'tripleDecr_eval1' and 'tripleDecr_eval2' yield
-- similar results (modulo types).

case_scope_local =
  (Right ((), (1::Integer)) @=? tripleDecr_eval1 tripleDecrLocal)
  >> (Right ((), (2::Integer)) @=? tripleDecr_eval1 tripleDecrIncrLocal)
  >> ((Right (), (1::Integer)) @=? tripleDecr_eval2 tripleDecrLocal)
  >> ((Right (), (2::Integer)) @=? tripleDecr_eval2 tripleDecrIncrLocal)

-- Ha! finally we get different scoping semantics when we modify 'catch'
-- catchError ==> catchError . (transactionState TxState)

-- Lesson: Non-algebraic operations (currently) require
-- redefinition/modification in order to alter their scoping semantics.

-- Lesson: For non-algebraic constructs (i.e., those that couple scoping with
-- semantics, in the sense that these operations don't commute with '>>='), we
-- need to:
-- 1. Manually pass them around as arguments, or
-- 2. Implement via Typeclasses (similar to 1), or
-- 3. Implement via Higher-order effects
-- 4. Implement via Via scoped syntax

-- catchError :: Member (Exc e) r => Eff r a -> (e -> Eff r a) -> Eff r a
-- transactionState :: Member (State s) r => TxState s -> Eff r a -> Eff r a

-- Challenges
-- 2. What type variables should the typeclass be indexed on, and how to convey
-- the intended meaning (scope vs not) at use-site?

data Symbol v where
  Symbol :: Char -> Symbol Char

symbol :: Member Symbol r => Char -> Eff r Char
symbol c = send (Symbol c)

digit :: [NDet, Symbol] <:: r => Eff r Char
digit = foldr mplus mzero (fmap symbol ['0'..'9'])

many p = mplus (many1 p) (return [])
many1 p = do
  a <- p
  as <- many p
  return (a:as)

parse :: Member NDet r
      => [Char] -> Eff (Symbol ': r) a -> Eff r a
parse [] (Val a) = return a
parse (x:xs) (Val a) = mzero
parse [] (E q (U0 (Symbol _))) = mzero
parse (x:xs) (E q (U0 (Symbol c)))
  | x == c = parse xs (q ^$ x)
  | otherwise = mzero
parse xs (E q (U1 u')) = relay (parse xs) q u'

expr :: [NDet, Symbol] <:: r => Eff r Int
expr = (do
           i <- term
           symbol '+'
           j <- expr
           return (i+j)
       ) `mplus` (do
           i <- term
           return i
       )
term = (do
           i <- factor
           symbol '*'
           j <- term
           return (i*j)
       ) `mplus` (do
                     i <- factor
                     return i
                 )
factor = (do
             ds <- many1 digit
             return (read ds)
         ) `mplus` (do
                       symbol '('
                       i <- expr
                       symbol ')'
                       return i)

runCase = run . makeChoiceA
case_parse_expr =
  [[42::Int]] @=? runCase ((sols . parse "2+8*5") expr)

expr1,expr2 :: [NDet, Symbol] <:: r => Eff r Int
expr1 = do
  i <- term
  (do
      symbol '+'
      j <- expr1
      return (i+j)
    ) `mplus` (do return i)
expr2 = do
  i <- term
  call ((do
            symbol '+'
            (!)
            j <- expr2
            return (i+j)
        ) `mplus` (return i))

-- case_parse_expr2 =
--   [[1]] @=? runCase ((sols . parse "1") expr2)

data CallBE v where
  BCall :: CallBE ()
  ECall :: CallBE ()

call' :: Member CallBE r => Eff r a -> Eff r a
call' p = do
  send BCall
  x <- p
  send ECall
  return x

type Cut = Exc CutFalse
expr3 :: [NDet, Symbol, CallBE, Cut] <:: r
      => Eff r Int
expr3 = do
  i <- term
  call' ((do
             symbol '+'
             (!)
             j <- expr3
             return (i+j)
         ) `mplus` (do return i))

bcall :: Member NDet r
      => Eff (CallBE ': Cut ': r) a -> Eff (Cut ': r) a
bcall (Val a) = return a
bcall (E q (U0 BCall)) = raise (call (ecall (q ^$ ()))) >>= bcall
bcall (E q (U0 ECall)) = error "Mismatched ECall!"
bcall (E q (U1 u')) = relay bcall q u'

ecall :: Member NDet r
      => Eff (CallBE ': Cut ': r) a
      -> Eff (Cut ': r) (Eff (CallBE ': Cut ': r) a)
ecall (Val a) = return (Val a)
ecall (E q (U0 BCall)) = raise (call (ecall (q ^$ ()))) >>= ecall
ecall (E q (U0 ECall)) = return (q ^$ ())
ecall (E q (U1 u')) = relay ecall q u'

runCut :: Member NDet r => Eff (CallBE ': Cut ': r) a -> Eff r a
runCut p = call (bcall p)

case_parse_expr3 =
  [[1]] @=? runCase ((sols . runCut . parse "1") expr3)

data CatchBE e v where
  BCatch :: (e -> a) -> CatchBE e ()
  ECatch :: CatchBE e ()

catch_ :: forall e r a. Member (CatchBE e) r => Eff r a -> (e -> Eff r a) -> Eff r a
catch_ p h = do
  send (BCatch h)
  x <- p
  send (ECatch @e)
  return x

runCatch :: Eff ((CatchBE e) ': (Exc e) ': r) a -> Eff r (Either e a)
runCatch p = runError (bcatch p)

bcatch :: Eff (CatchBE e : Exc e : r) a -> Eff (Exc e : r) a
bcatch (Val a) = return a
bcatch (E q (U0 (BCatch h))) = do
  r <- raise (runError (ecatch (q ^$ ())))
  case r of
    Left e -> undefined -- bcatch (h e)
    Right p -> bcatch p
bcatch (E q (U0 ECatch)) = error "Mismatched ECatch!"
bcatch (E q (U1 u)) = relay bcatch q u

ecatch :: Eff (CatchBE e : Exc e : r) a
       -> Eff (Exc e : r) (Eff (CatchBE e : Exc e : r) a)
ecatch (Val a) = return (Val a)

type Effs = [(* -> *) -> * -> *]
class UnionHO u t (r :: Effs) m v where
  inj :: t m v -> u r m v
  prj :: u r m v -> Maybe (t m v)
  decomp :: u (t ': r) m v -> Either (u r m v) (t m v)

class EffHO q e (r :: Effs) a where
  val :: a -> e r a
  eff :: u r (e r) v -> q (e r) v a -> e r a

class H q m t (r :: Effs) a k where
  oph :: t m v -> q m v a -> (m a -> k) -> k
  relh :: u r m v -> q m v a -> (m a -> k) -> k

effh :: H q (e (t ': r)) t r a k
     => UnionHO u t r (e (t ': r)) v
     => e (t ': r) a
     -> (a -> k)
     -> (e (t ': r) a -> k)
     -> k
effh m ret self = undefined

sendHO :: t m v -> (u r m v, q m v a)
sendHO = undefined


--newtype Throw e v = Throw e
-- data Catch e v where
--   Catch :: (r ~ ((Throw e) ': r')) => Eff r x -> (e -> Eff r x) -> Catch e (Eff r x)

-- FIXME: HC5 doesn't work without allowing explicit access to r' below
data Catch e r v where
  Catch :: Member (Exc e) r => Eff r x -> (e -> Eff r x) -> Catch e r (Eff r x)

-- catch_req type gets inferred
catch_req :: Member (Catch e r1) r2
          => Member (Exc e) r1
          => Eff r1 x -> (e -> Eff r1 x)
          -> Eff r2 (Eff r1 x)
catch_req m eh = send (Catch m eh)

-- NOTE the need for return type to be 'Either'. It is necessitated by HC1.
-- also handle_catch signature needs signature. Limits of GHC type inference.
handle_catch :: Open (Eff ((Catch e (Exc e ': r)) ': r) (Eff ((Exc e ': r)) a) -> Eff r (Either e a))
handle_catch self (Val x) = case x of
  Val a -> Val (Right a)
  E _ (U0 (Exc e)) -> Val (Left e) -- HC1. FIXME: Could this be statically
                                   -- eliminated? Would adding a scoped
                                   -- constructor for Eff help? But then what if
                                   -- we need yet another level?
  E q (U1 u) -> relay self (qComps q Val) u
handle_catch self (E q (U0 (Catch m eh))) = case m of
  Val a -> (self<.>q) (Val a)
  E _ (U0 (Exc e)) -> (self<.>q) (eh e) -- HC4
  E q' (U1 u') -> relayK (self<.>q<.>q') u' -- HC5
handle_catch self (E q (U1 u)) = relay self q u

-- FIXME: handling either of Exc or State before Catch means that inner level
-- isn't simply an "Eff r a", but something more complex. How can handle_catch
-- made to accommodate these cases? Or would each outer effect linearization
-- require a different handle_catch?
--
-- NOTE: We have State at two levels. How do we merge them? How we choose to
-- merge the two states is the essence of the conundrum posed by the tripleDecr
-- program.
tripleDecrNested :: [Catch () r1, Exc (), State Integer] <:: r2
                 => [Exc (), State Integer] <:: r1
                 => Eff r2 (Eff r1 ())
tripleDecrNested = decr >> (catch_req (decr >> decr) return)
-- NOTE: interestingly, using prog doesn't generate the signature that we
-- want. it generates:
--
-- tripleDecrNested :: Member (Catch () r') r'
--                  => Member (State Integer) r')
--                  => Eff (Exc () : r') (Eff (Exc () : r') ())

{-
prog :: Monad m => (m b1 -> t -> m b2) -> m b1 -> t -> m b2
prog catch d r = d >> catch (d >> d) r
-}

-- TODO: specify a way to thread handle_catch through other handlers. Something like:
--
-- handle_catch3 :: Monad c
--                => Open (Eff ((Catch e (Exc e ': r)) ': r) (c (Eff ((Exc e ': r)) a)) -> Eff r (c a))
--
-- Perhaps 'c' shouldn't be a Monad. It's some sort of context. Perhaps Functor
-- is all we can say about it? One is reminded of 'handle' from Handlers in
-- Scope: 
--
-- handle :: (Monad m, Monad n, Functor c)
--        => c () -> (forall x. c (m x) -> n (c x)) -> (sig m a -> sig n (c a))

tripleDecrNested_res = runError $ tripleDecrNested
{-
tripleDecr_eval1 :: Eff '[State Integer, Fail] () -> Either () ((), Integer)
tripleDecr_eval1 = run . runError . (runState (2::Integer))
case_scope_global =
  (Right ((), (0::Integer)) @=? tripleDecr_eval1 tripleDecr)
-}

-- class HandleScope t r a k where
--   -- | Define a single step of computation for the handler, i.e., define how to
--   -- "handle" the effectful request.
--   --
--   -- A handler is a function which accepts an effectful computation as an
--   -- argument. A "single step" includes responding to the awaiting coroutine and
--   -- passing the result to the handler reference.
--   handle_scope :: (Eff r a -> k) -- ^ handler reference
--                -> Arrs r v a -- ^ coroutine awaiting response from request
--                -> t v -- ^ effect request
--                -> k -- ^ the result type after effect is handled
