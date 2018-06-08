{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This module contains several tiny examples of how to use effects.
--
-- @
-- import Control.Eff
-- import Control.Eff.Reader.Lazy
-- import Control.Eff.Writer.Lazy
-- import Control.Eff.State.Lazy
-- import Control.Eff.Exception
-- @

module Control.Eff.QuickStart where

import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Control.Eff.State.Lazy
import           Control.Eff.Exception
import           Control.Monad                            ( when )


-- | an effectful function that can throw an error
--
-- @
-- tooBig = do
--   when (i > 100) $ throwError $ show i
--   return i
-- @
tooBig :: Member (Exc String) r => Int -> Eff r Int
tooBig i = do
  when (i > 100) $ throwError $ show i
  return i

-- | run the @tooBig@ effect based on a provided Int.
--
-- @
-- runTooBig i = run . runError $ tooBig i
-- @
runTooBig :: Int -> Either String Int
runTooBig i = run . runError $ tooBig i

-- | run the @tooBig@ effect, giving @1@ as input
--
-- @
-- runTooBig1 = runTooBig 1
-- -- Right 1
-- @
runTooBig1 :: Either String Int
runTooBig1 = runTooBig 1 -- Right 1

-- | run the @tooBig@ effect, giving @200@ as input, which is too big for this
-- effect.
--
-- @
-- runTooBig1 = runTooBig 200
-- -- Left "200"
-- @
runTooBig200 :: Either String Int
runTooBig200 = runTooBig 200 -- Left "200"

-- | an effectul computation using state. The state is of type @[Int]@.
-- This function takes the head off the list, if it is there and return it.
-- If state is the empty list, then it stays the same and returns @Nothing@.
--
-- @
-- popState = do
--  stack <- get
--  case stack of
--    []       -> return Nothing
--    (x : xs) -> do
--      put xs
--      return $ Just x
-- @
popState :: Member (State [Int]) r => Eff r (Maybe Int)
popState = do
  stack <- get
  case stack of
    []       -> return Nothing
    (x : xs) -> do
      put xs
      return $ Just x

-- | run the popState effectful computation based on initial state. The
-- result-type is the result of the computation @Maybe Int@ together with the
-- state at the end of the computation @[Int]@
--
-- @
-- runPopState xs = run . runState xs $ popState
-- @
runPopState :: [Int] -> (Maybe Int, [Int])
runPopState xs = run . runState xs $ popState

-- | run the popState effectful function
--
-- @
-- runPopState123 = runPopState  [1, 2, 3]
-- -- (Just 1, [2, 3])
-- @
runPopState123 :: (Maybe Int, [Int])
runPopState123 = runPopState [1, 2, 3] -- (Just 1, [2, 3])

-- | run the popState effectful function
--
-- @
-- runPopStateEmpty = runPopState []
-- -- (Nothing, [])
-- @
runPopStateEmpty :: (Maybe Int, [Int])
runPopStateEmpty = runPopState [] -- (Nothing, [])

-- | an effect that returns a number one more than the given
--
-- @
-- oneMore = do
--   x <- ask -- query the environment
--   return $ x + 1 -- add one to the asked value and return it
-- @
oneMore :: Member (Reader Int) r => Eff r Int
oneMore = do
  x <- ask
  return $ x + 1

-- | Run the @oneMore@ effectful function by giving it a value to read.
--
-- @
-- runOneMore = run . runReader 1 $ oneMore
-- -- 2
-- @
-- TODO: runOneMore function and separate runOneMore1
runOneMore1 :: Int
runOneMore1 = run . runReader 1 $ oneMore -- 2

-- | An effectful computation with multiple effects:
--
-- * A value gets read
-- * an error can be thrown depending on the read value
-- * state gets read and transformed
--
-- All these effects are composed using the @Eff@ monad using the corresponding
-- Effect types.
--
-- @
-- something = do
--   readValue :: Float <- ask -- read a value from the environment
--   when (readValue < 0) $ throwError readValue  -- if the value is negative, throw an error
--   modify (\l -> (round readValue :: Integer) : l) -- add the rounded read element to the list
--   currentState :: [Integer] <- get -- get the state after the modification
--   return $ sum currentState -- sum the elements in the list and return that
-- @
something
  :: (Member (Reader Float) r, Member (State [Integer]) r, Member (Exc Float) r)
  => Eff r Integer
something = do
  readValue :: Float <- ask
  when (readValue < 0) $ throwError readValue
  modify (\l -> (round readValue :: Integer) : l)
  currentState :: [Integer] <- get
  return $ sum currentState

-- | Run the @someting@ effectful computation given in the previous function.
-- The handlers apply from bottom to top - so this is the reading direction.
--
-- @
-- runSomething1 initialState newValue =
--   run . -- run the Eff-monad with no effects left
--   runError . -- run the error part of the effect. This introduces the Either in the result.
--   runState initialState . -- handle the state-effect providing an initial state giving back a pair.
--   runReader newValue $ -- provide the computation with the dynamic value to read/ask for
--   something -- the computation - function
-- @
runSomething1 :: [Integer] -> Float -> Either Float (Integer, [Integer])
runSomething1 initialState newValue =
  run . runError . runState initialState . runReader newValue $ something

-- | Run the @something@ effectful computation given above.
-- This has an alternative ordering of the effect-handlers.
--
-- The used effect-handlers are the same are used in slightly different order:
-- The @runState@ and @runError@ methods are swapped, which results in a
-- different output type and run-semantics.
--
-- @
-- runSomething1 initialState newValue =
--   run .
--   runState initialState .
--   runError .
--   runReader newValue $
--   something -- the computation - function
-- @
runSomething2 :: [Integer] -> Float -> (Either Float Integer, [Integer])
runSomething2 initialState newValue =
  run . runState initialState . runError . runReader newValue $ something

-- | run the @runSomething1@ functin with @[]@ and @(-0.5)@ as parameters
--
-- @
-- runSomething1InputNegative = runSomething1 [] (-0.5) -- Left (-0.5)
-- @
runSomething1InputNegative :: Either Float (Integer, [Integer])
runSomething1InputNegative = runSomething1 [] (-0.5) -- Left (-0.5)

-- | run the @runSomething1@ functin with @[2]@ and @1.3@ as parameters
--
-- @
-- runSomething1InputPositive = runSomething1 [2] 1.3 -- Right (3, [1,2])
-- @
runSomething1InputPositive :: Either Float (Integer, [Integer])
runSomething1InputPositive = runSomething1 [2] 1.3 -- Right (3, [1,2])

-- | run the @runSomething2@ functin with @[4]@ and @(-2.4)@ as parameters
--
-- @
-- runSomething2InputNegative = runSomething2 [4] (-2.4) -- (Left (-2.4), [4])
-- @
runSomething2InputNegative :: (Either Float Integer, [Integer])
runSomething2InputNegative = runSomething2 [4] (-2.4) -- (Left (-2.4), [4])

-- | run the @runSomething2@ functin with @[4]@ and @5.9@ as parameters
--
-- @
-- runSomething2InputPositive = runSomething2 [4] 5.9 -- (Right 10, [6,4])
-- @
runSomething2InputPositive :: (Either Float Integer, [Integer])
runSomething2InputPositive = runSomething2 [4] 5.9 -- (Right 10, [6,4])
