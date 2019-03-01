module DoctestRun where

import Test.DocTest (doctest)

runDocTest :: IO ()
runDocTest = do
  putStrLn ""
  putStrLn ""
  putStrLn "Doc Test..."
  doctest [ -- pass default-extensions to ghc
    "-XDataKinds"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XMultiParamTypeClasses"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XTypeOperators"
    , "src/"
    ]
  putStrLn "Doc Test OK"
  putStrLn ""
