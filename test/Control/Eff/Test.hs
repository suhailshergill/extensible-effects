{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Test (testGroups) where

import Test.HUnit hiding (State)
import Test.QuickCheck
import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Exception
import qualified Control.Exception as Exc
import Utils

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

testGroups = [ $(testGroupGenerator) ]

prop_NestedEff :: Property
prop_NestedEff = forAll arbitrary (\x -> property (qu x == x))
  where
    qu :: Bool -> Bool
    qu x = run $ runReader readerId (readerAp x)

    readerAp :: Bool -> Eff '[Reader (Eff '[Reader Bool] Bool)] Bool
    readerAp x = do
      f <- ask
      return . run $ runReader x f

    readerId :: Eff '[Reader Bool] Bool
    readerId = do
      x <- ask
      return x

-- | Ensure that https://github.com/RobotGymnast/extensible-effects/issues/11 stays resolved.
case_Lift_building :: Assertion
case_Lift_building = runLift possiblyAmbiguous
  where
    possiblyAmbiguous :: (Monad m, Lifted m r) => Eff r ()
    possiblyAmbiguous = lift $ return ()

case_Lift_tl1r :: Assertion
case_Lift_tl1r = do
  ((), output) <- catchOutput tl1r
  assertEqual "Test tl1r" (showLn input) output
  where
    input = (5::Int)
    -- tl1r :: IO ()
    tl1r = runLift (runReader input tl1)
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

    tMd' = runLift $ runReader val $ mapMdebug' f input
      where f x = ask `add` return x

    -- Re-implemenation of mapMdebug using Lifting
    -- The signature is inferred
    mapMdebug'  :: (Show a, Lifted IO r) =>
                   (a -> Eff r b) -> [a] -> Eff r [b]
    mapMdebug' _f [] = return []
    mapMdebug' f (h:t) = do
      lift $ print h
      h' <- f h
      t' <- mapMdebug' f t
      return (h':t')

-- tests from <http://okmij.org/ftp/Haskell/misc.html#catch-MonadIO>
data MyException = MyException String deriving (Show)
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
    tf x = runReader (x::Bool) . runState ([]::[String]) $ testc m
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
    tf x = runReader (x::Bool) . runState ([]::[String]) $ runErrorStr (testc m)
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
    tf x = runReader (x::Bool) . runState ([]::[String]) $ runErrorStr (testc m)
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
    tf x = runReader (x::Bool) . runState ([]::[String]) $ runErrorStr (testc m)
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
    tf x = runReader (x::Bool) . runState ([]::[String]) $ runErrorStr (testc m)
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
    tf x = runReader (x :: Bool) . runState ([]::[String]) $ m1
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
