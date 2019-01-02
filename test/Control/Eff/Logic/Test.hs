{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Logic.Test where

import Test.HUnit hiding (State)
import Control.Eff.Logic.Core
import Control.Monad

-- the inferred signature of testCut is insightful
testCut runChoice =
  let cases = [tcut1, tcut2, tcut3, tcut4, tcut5, tcut6, tcut7, tcut8
              , tcut9]
      runCall = runChoice . call
  in
    forM_ cases $ \(test, result) ->
                    assertEqual "Cut: tcut" result (runCall test)
  where
    -- signature is inferred
    -- tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
    tc1 = (return (1::Int) `mplus` return 2) `mplus`
          ((cutfalse `mplus` return 4) `mplus`
            return 5)
    rc1 = [1,2]
    tcut1 = (tc1, rc1)
    -- Here we see nested call. It poses no problems...
    tc2 = return (1::Int) `mplus`
          call (return 2 `mplus` (cutfalse `mplus` return 3) `mplus`
                 return 4)
          `mplus` return 5
    rc2 = [1,2,5]
    tcut2 = (tc2, rc2)
    tcut3 = ((call tc1 `mplus` call (tc2 `mplus` cutfalse))
            , rc1 ++ rc2)
    tcut4 = ((call tc1 `mplus`  (tc2 `mplus` cutfalse))
            , rc1 ++ rc2)
    tcut5 = ((call tc1 `mplus`  (cutfalse `mplus` tc2))
            , rc1)
    tcut6 = ((call tc1 `mplus` call (cutfalse `mplus` tc2))
            , rc1)
    tcut7 = ((call tc1 `mplus`  (cutfalse `mplus` tc2) `mplus` tc2)
            , rc1)
    tcut8 = ((call tc1 `mplus` call (cutfalse `mplus` tc2) `mplus` tc2)
            , rc1 ++ rc2)
    incrOrDecr = \x -> (return $! x + 1)
                       `mplus` cutfalse
                       `mplus` (return $! x - 1)
    tc9 = tc1 >>= incrOrDecr
    rc9 = [2]
    tcut9 = (tc9, rc9)
    -- tcut10 = ((return rc1 >>= incrOrDecr)
    --          , rc9)
