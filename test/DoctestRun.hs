module DoctestRun where

import Test.DocTest (doctest)

runDocTest :: IO ()
runDocTest = do
  putStrLn ""
  putStrLn ""
  putStrLn "Doc Test..."
  doctest ["-i", "-XFlexibleContexts", "-XMultiParamTypeClasses", "-XFlexibleInstances", "-XGADTs", "-XScopedTypeVariables",
           "-isrc", "src/Control/Eff/QuickStart.hs"]
  putStrLn "Doc Test OK"
  putStrLn ""
