{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Eff.Logic.Experimental where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Exception
import Control.Eff.Logic.Core
import Control.Monad

instance (MonadPlus (Eff (Exc CutFalse : r)), MSplit (Eff (Exc CutFalse : r)))
  => Call r where
  call m = loop m [] where
    loop m' jq = case msplit m' of
      Val Nothing       -> next jq                        -- (C1)
      Val (Just (x, q)) -> return x `mplus` next (q : jq) -- (C2)
      E q u -> case u of
        U0 (Exc CutFalse) -> next []                      -- drop jq (F2)
        U1 _              -> loop (E q u >>= reflect) jq  -- (C4?)
        --_                 -> loop m' jq
    next jq = list mzero loop jq                          -- (C3?)
  {-
  call m = loop (msplit m) [] where
    loop (Val Nothing) jq       = next jq                        -- (C1)
    loop (Val (Just (x, q))) jq = return x `mplus` next (q : jq) -- (C2)
    loop (E q u) jq             = case u of
      U0 (Exc CutFalse)        -> next []                        -- drop jq (F2)
      _                        -> loop (E q u) jq          -- (C4?)

    next []    = mzero
    next (h:t) = loop (msplit h) t                               -- (C3?)
  -}
