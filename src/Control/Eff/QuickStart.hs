{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | This module contains several tiny examples of how to use effects.
-- For technical details, see the documentation in the effect-modules.
--
-- Note that most examples given here are very small. For them,
-- using `Eff` monad is more complicated compared to a standard functional
-- approach.
-- The power of extensible effects lie in the fact that these computations can
-- be used to construct much more complicated programs by composing the little
-- pieces shown here.
--
-- This module imports and reexports modules from this library and requires
-- some language extensions:
--
-- @
-- {-\# LANGUAGE ScopedTypeVariables \#-}
-- {-\# LANGUAGE FlexibleContexts \#-}
-- {-\# LANGUAGE MonoLocalBinds \#-}
--
-- import Control.Eff
-- import Control.Eff.Reader.Lazy
-- import Control.Eff.Writer.Lazy
-- import Control.Eff.State.Lazy
-- import Control.Eff.Exception
-- @
--
-- If you want to see what each extension is good for, you can disable it and
-- see what GHC will complain about.
--
module Control.Eff.QuickStart
  ( -- * Examples
    module Control.Eff.QuickStart
    -- * Imported effect modules
  , module Control.Eff.Reader.Lazy
  , module Control.Eff.State.Lazy
  , module Control.Eff.Exception
  ) where

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
--
-- >>> runTooBig 1
-- Right 1
--
-- >>> runTooBig 200
-- Left "200"
runTooBig :: Int -> Either String Int
runTooBig i = run . runError $ tooBig i

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
--
-- >>> runPopState  [1, 2, 3]
-- (Just 1,[2,3])
--
-- >>> runPopState []
-- (Nothing,[])
runPopState :: [Int] -> (Maybe Int, [Int])
runPopState xs = run . runState xs $ popState

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
-- runOneMore i = run . runReader i $ oneMore
-- @
--
-- >>> runOneMore 1
-- 2
runOneMore :: Int -> Int
runOneMore i = run . runReader i $ oneMore

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
--
-- >>> runSomething1 [] (-0.5)
-- Left (-0.5)
--
-- >>> runSomething1 [2] 1.3
-- Right (3,[1,2])
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
--
-- >>> runSomething2 [4] (-2.4)
-- (Left (-2.4),[4])
--
-- >>> runSomething2 [4] 5.9
-- (Right 10,[6,4])
runSomething2 :: [Integer] -> Float -> (Either Float Integer, [Integer])
runSomething2 initialState newValue =
  run . runState initialState . runError . runReader newValue $ something
