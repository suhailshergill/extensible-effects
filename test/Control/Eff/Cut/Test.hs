{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Eff.Cut.Test (testGroups) where

import Test.HUnit hiding (State)
import Control.Eff
import Control.Eff.Choose
import Control.Eff.Cut

import Test.Framework.TH
import Test.Framework.Providers.HUnit

testGroups = [ $(testGroupGenerator) ]

case_Cut_tcut :: Assertion
case_Cut_tcut =
  let tcut1r = run . makeChoice $ call tcut1
      tcut2r = run . makeChoice $ call tcut2
      tcut3r = run . makeChoice $ call tcut3
      tcut4r = run . makeChoice $ call tcut4
  in
    assertEqual "Cut: tcut1" [1,2] tcut1r
    >> assertEqual "Cut: nested call: tcut2" [1,2,5] tcut2r
    >> assertEqual "Cut: nested call: tcut3" [1,2,1,2,5] tcut3r
    >> assertEqual "Cut: nested call: tcut4" [1,2,1,2,5] tcut4r
  where
    -- signature is inferred
    -- tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
    tcut1 = (return (1::Int) `mplus'` return 2) `mplus'`
            ((cutfalse `mplus'` return 4) `mplus'`
             return 5)
    -- Here we see nested call. It poses no problems...
    tcut2 = return (1::Int) `mplus'`
            call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'`
                  return 4)
            `mplus'` return 5
    tcut3 = call tcut1 `mplus'` call (tcut2 `mplus'` cutfalse)
    tcut4 = call tcut1 `mplus'`  (tcut2 `mplus'` cutfalse)
