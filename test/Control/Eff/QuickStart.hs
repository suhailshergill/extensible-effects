module Control.Eff.QuickStart where


import Control.Eff
import Control.Eff.Writer.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Exception

oneMore :: Member (Reader Int) r -> Eff r Int
oneMore = do
    x <- ask
    return $ x + 1

runOneMore1 = run . runReader 1 $ oneMore -- 2

tooBig :: Member (Exc String) r => Int -> Eff r Int
tooBix i = do
  if i > 100 then throwError $ show i else return i

runTooBig :: Int -> Either Int Int
runTooBig i = run . runExc $ tooBig i


runTooBig1 = runTooBig 1 -- Right 1
runTooBig200 = runTooBig 200 -- Left "200"

popState :: Member (State [Int]) r => Eff r (Maybe Int)
popState = do
  stack <- get
  case stack of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return x

runPopState :: [Int] -> (Maybe Int, [Int])
runPopState xs = runState xs popState

runPopState123 = runPopState [1, 2, 3] -- (Just 1, [2, 3])
runPopStateEmpty = runPopState [] -- (Nothing, [])
