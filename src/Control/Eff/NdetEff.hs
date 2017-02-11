{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

-- | Another implementation of nondeterministic choice effect
module Control.Eff.NdetEff where

import qualified Control.Eff1 as E1
import Data.OpenUnion51
import Data.FTCQueue1

import Control.Monad
import Control.Applicative

-- | A different implementation, more directly mapping to MonadPlus
-- interface
data NdetEff a where
  MZero :: NdetEff a
  MPlus :: NdetEff Bool

-- FIXME: uncomment
-- instance Member NdetEff r => Alternative (E1.Eff r) where
--   empty = mzero
--   (<|>) = mplus

-- instance Member NdetEff r => MonadPlus (E1.Eff r) where
--   mzero = E1.send MZero
--   mplus m1 m2 = E1.send MPlus >>= \x -> if x then m1 else m2

-- | An interpreter
-- The following is very simple, but leaks a lot of memory
-- The cause probably is mapping every failure to empty
-- It takes then a lot of timne and space to store those empty
runChoiceA0 :: Alternative f => E1.Eff (NdetEff ': r) a -> E1.Eff r (f a)
runChoiceA0 = E1.handle_relay (return . pure) $ \m k -> case m of
    MZero -> return empty
    MPlus -> liftM2 (<|>) (k True) (k False)

-- | A different implementation, more involved but faster and taking
-- much less (100 times) less memory.
-- The benefit of the effect framework is that we can have many
-- interpreters.
runChoiceA :: Alternative f => E1.Eff (NdetEff ': r) a -> E1.Eff r (f a)
runChoiceA m = loop [] m
 where
   loop [] (E1.Val x)    = return (pure x)
   loop (h:t) (E1.Val x) = loop t h >>= \r -> return (pure x <|> r)
   loop jq (E1.E u q) = case  decomp u of
     Right MZero     -> case jq of
       []    -> return empty
       (h:t) -> loop t h
     Right MPlus -> loop (E1.qApp q False : jq) (E1.qApp q True)
     Left  u -> E1.E u (tsingleton (\x -> loop jq (E1.qApp q x)))
