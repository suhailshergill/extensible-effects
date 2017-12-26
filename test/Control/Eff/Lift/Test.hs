{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Lift.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import qualified Control.Exception as Exc
import Data.Typeable
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Monad m, SetMember Lift (Lift m) r) => Eff r ()
    possiblyAmbiguous = lift $ return ()

case_Lift_tl1r :: Assertion
case_Lift_tl1r = do
  ((), output) <- catchOutput tl1r
  assertEqual "Test tl1r" (showLn input) output
  where
    input = (5::Int)
    -- tl1r :: IO ()
    tl1r = runLift (runReader tl1 input)
      where
        tl1 = ask >>= \(x::Int) -> lift . print $ x

case_Lift_tMd' :: Assertion
case_Lift_tMd' = do
  actual <- catchOutput tMd'
  let expected = (output, (showLines input))
  assertEqual "Test mapMdebug using Lift" expected actual
  where
    input = [1..5]
    val = (10::Int)
    output = map (+ val) input

    tMd' = runLift $ runReader (mapMdebug' f input) val
      where f x = ask `add` return x

    -- Re-implemenation of mapMdebug using Lifting
    -- The signature is inferred
    mapMdebug'  :: (Show a, SetMember Lift (Lift IO) r) =>
                   (a -> Eff r b) -> [a] -> Eff r [b]
    mapMdebug' _f [] = return []
    mapMdebug' f (h:t) = do
      lift $ print h
      h' <- f h
      t' <- mapMdebug' f t
      return (h':t')

-- tests from <http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO>
data MyException = MyException String deriving (Show, Typeable)
instance Exc.Exception MyException

exfn True = lift . Exc.throw $ (MyException "thrown")
exfn False = return True

testc m = catchDynE (m >>= return . show) (\ (MyException s) -> return s)

case_catchDynE_test1 :: Assertion
case_catchDynE_test1 = do
  ((), actual) <- catchOutput test1
  let expected = unlines [ "(\"thrown\",[\"begin\"])"
                         , "(\"True\",[\"end\",\"begin\"])"]
  assertEqual "catchDynE: test1: exception shouldn't drop Writer's state"
    expected actual
  where
    -- In CatchMonadIO, the result of tf True is ("thrown",[]) --
    -- that is, an exception will drop the Writer's state, even if that
    -- exception is caught. Here, the state is preserved!
    -- So, this is an advantage over MTL!
    test1 = do runLift (tf True) >>= print; runLift (tf False) >>= print
    tf x = runReader (runState (testc m) ([]::[String])) (x::Bool)
    m = do
      modify ("begin":)
      x <- ask
      r <- exfn x
      modify ("end":)
      return r

-- Let us use an Error effect instead
case_catchDynE_test1' :: Assertion
case_catchDynE_test1' = do
  ((), actual') <- catchOutput test1'
  let expected' = unlines [ "(Left \"thrown\",[\"begin\"])"
                         , "(Right \"True\",[\"end\",\"begin\"])"]
  assertEqual "catchDynE: test1': Error shouldn't drop Writer's state"
    expected' actual'
  where
    -- In CatchMonadIO, the result of tf True is ("thrown",[]) --
    -- that is, an exception will drop the Writer's state, even if that
    -- exception is caught. Here, the state is preserved!
    -- So, this is an advantage over MTL!
    test1' = do runLift (tf True) >>= print; runLift (tf False) >>= print
    tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
    m = do
      modify ("begin":)
      x <- ask
      r <- exfn x
      modify ("end":)
      return r

    runErrorStr = asEStr . runError
    asEStr :: m (Either String a) -> m (Either String a)
    asEStr = id
    exfn True = throwError $ ("thrown")
    exfn False = return True

-- Now, the behavior of the dynamic Exception and Error effect is consistent.
-- The state is preserved. Before it wasn't.
case_catchDynE_test2 :: Assertion
case_catchDynE_test2 = do
  ((), actual) <- catchOutput test2
  let expected = unlines [ "(Left \"thrown\",[\"begin\"])"
                         , "(Right \"True\",[\"end\",\"begin\"])"]
  assertEqual "catchDynE: test2: Error shouldn't drop Writer's state"
    expected actual
  where
    test2 = do runLift (tf True) >>= print; runLift (tf False) >>= print
    tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
    runErrorStr = asEStr . runError
    asEStr :: m (Either String a) -> m (Either String a)
    asEStr = id
    m = do
      modify ("begin":)
      x <- ask
      r <- exfn x `catchDynE` (\ (MyException s) -> throwError s)
      modify ("end":)
      return r

-- Full recovery
case_catchDynE_test2' :: Assertion
case_catchDynE_test2' = do
  ((), actual) <- catchOutput test2'
  let expected = unlines [ "(Right \"False\",[\"end\",\"begin\"])"
                         , "(Right \"True\",[\"end\",\"begin\"])"]
  assertEqual "catchDynE: test2': Fully recover from errors"
    expected actual
  where
    test2' = do runLift (tf True) >>= print; runLift (tf False) >>= print
    tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
    runErrorStr = asEStr . runError
    asEStr :: m (Either String a) -> m (Either String a)
    asEStr = id
    m = do
      modify ("begin":)
      x <- ask
      r <- exfn x `catchDynE` (\ (MyException _s) -> return False)
      modify ("end":)
      return r

-- Throwing within a handler
case_catchDynE_test3 :: Assertion
case_catchDynE_test3 = do
  ((), actual) <- catchOutput test3
  let expected = unlines [ "(Right \"rethrow:thrown\",[\"begin\"])"
                         , "(Right \"True\",[\"end\",\"begin\"])"]
  assertEqual "catchDynE: test3: Throwing within a handler"
    expected actual
  where
    test3 = do runLift (tf True) >>= print; runLift (tf False) >>= print
    tf x = runReader (runState (runErrorStr (testc m)) ([]::[String])) (x::Bool)
    runErrorStr = asEStr . runError
    asEStr :: m (Either String a) -> m (Either String a)
    asEStr = id
    m = do
      modify ("begin":)
      x <- ask
      r <- exfn x `catchDynE` (\ (MyException s) ->
                                 lift . Exc.throw . MyException $
                                 ("rethrow:" ++ s))
      modify ("end":)
      return r

-- Implement the transactional behavior: when the exception is raised,
-- the state is rolled back to what it existed at the entrance to
-- the catch block.
-- This is the ``scoping behavior'' of `Handlers in action'
case_catchDynE_tran :: Assertion
case_catchDynE_tran = do
  ((), actual) <- catchOutput tran
  let expected = unlines ["(\"thrown\",[\"init\"])"
                         ,"(\"True\",[\"end\",\"begin\",\"init\"])"]
  assertEqual "catchDynE: tran: Transactional behaviour"
    expected actual
  where
    tran = do runLift (tf True) >>= print; runLift (tf False) >>= print
    tf x = runReader (runState m1 ([]::[String])) (x::Bool)
    m1 = do
      modify ("init":)
      testc (transactionState (TxState :: TxState [String]) m)
    m = do
      modify ("begin":)
      x <- ask
      r <- exfn x
      modify ("end":)
      return r
{- -- without transaction
("thrown",["begin","init"])
("True",["end","begin","init"])
-}

-- With transaction
{-
("thrown",["init"])
("True",["end","begin","init"])
-}
