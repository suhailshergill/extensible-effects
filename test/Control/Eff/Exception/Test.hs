{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Control.Eff.Exception.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Writer.Strict
import Control.Eff.State.Strict
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

-- The type is inferred
-- et1 :: Eff r Int
et1 = return 1 `add` return 2

case_Exception1_et1 :: Assertion
case_Exception1_et1 = 3 @=? (run et1)

-- The type is inferred
-- et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwError (2::Int)

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    Could not deduce (Data.OpenUnion.FindElem (Exc Int) '[])
      arising from a use of `et2'
-}

case_Exception1_et21 :: Assertion
case_Exception1_et21 = (Left (2::Int)) @=?
  (run et21)
  where
    -- The inferred type shows that ex21 is now pure
    -- et21 :: Eff r (Either Int Int)

    et21 = runError et2

-- Implementing the operator <|> from Alternative:
--  a <|> b does
--   -- tries a, and if succeeds, returns its result
--   -- otherwise, tries b, and if succeeds, returns its result
--   -- otherwise, throws mappend of exceptions of a and b

-- We use SetMember in the signature rather than Member to
-- ensure that the computation throws only one type of exceptions.
-- Otherwise, this construction is not very useful.
alttry :: forall e r a. (Monoid e, SetMember Exc (Exc e) r) =>
          Eff r a -> Eff r a -> Eff r a
alttry ma mb =
  catchError ma $ \ea ->
  catchError mb $ \eb -> throwError (mappend (ea::e) eb)

case_Exception1_alttry :: Assertion
case_Exception1_alttry =
  [Right 10,Right 10,Right 10,Left "bummer1bummer2"] @=?
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

case_Failure1_Effect :: Assertion
case_Failure1_Effect =
  let go :: Eff (Exc () ': Writer Int ': '[]) Int
         -> Int
      go = snd . run . runWriter (+) 0 . ignoreFail
      ret = go $ do
        tell (1 :: Int)
        tell (2 :: Int)
        tell (3 :: Int)
        () <- die
        tell (4 :: Int)
        return 5
   in assertEqual "Fail should stop writing" 6 ret

case_Exception1_monadBaseControl :: Assertion
case_Exception1_monadBaseControl =
    runLift (runError act) @=? Just (Left "Fail")
  where
    act = doThing $ do _ <- throwError "Fail"
                       return "Success"




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
