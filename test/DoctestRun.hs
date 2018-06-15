module DoctestRun where

import Test.DocTest (doctest)

runDocTest :: IO ()
runDocTest = do
  putStrLn ""
  putStrLn ""
  putStrLn "Doc Test..."
  doctest ["-i", "-XFlexibleContexts", "-XMultiParamTypeClasses", "-XFlexibleInstances", "-XGADTs", "-XScopedTypeVariables"]
  putStrLn "Doc Test OK"
  putStrLn ""
