{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains several tiny examples of how to use effects.
--
-- @
--     import Control.Eff
--     import Control.Eff.Reader.Lazy
--     import Control.Eff.Writer.Lazy
--     import Control.Eff.State.Lazy
--     import Control.Eff.Exception
-- @

module Control.Eff.QuickStart where

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Exception


-- | an effect that returns a number one more than the given
oneMore :: Member (Reader Int) r => Eff r Int
oneMore = do
    x <- ask
    return $ x + 1


-- | @asdf@ `asdf`
runOneMore1 :: Int
runOneMore1 = run . runReader 1 $ oneMore -- 2

tooBig :: Member (Exc String) r => Int -> Eff r Int
tooBig i = do
  if i > 100 then throwError $ show i else return i

runTooBig :: Int -> Either String Int
runTooBig i = run . runError $ tooBig i

runTooBig1 :: Either String Int
runTooBig1 = runTooBig 1 -- Right 1
runTooBig200 :: Either String Int
runTooBig200 = runTooBig 200 -- Left "200"

popState :: Member (State [Int]) r => Eff r (Maybe Int)
popState = do
  stack <- get
  case stack of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return $ Just x

runPopState :: [Int] -> (Maybe Int, [Int])
runPopState xs = run . runState xs $ popState

runPopState123 :: (Maybe Int, [Int])
runPopState123 = runPopState [1, 2, 3] -- (Just 1, [2, 3])
runPopStateEmpty :: (Maybe Int, [Int])
runPopStateEmpty = runPopState [] -- (Nothing, [])
