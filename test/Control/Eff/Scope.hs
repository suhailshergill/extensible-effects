{-# OPTIONS_GHC -Wmissing-signatures #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Eff.Scope where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Extend hiding (eff)
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
