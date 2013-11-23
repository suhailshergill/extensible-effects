{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Strict state effect
--
-- Example: implementing `Control.Eff.Fresh`
--
-- > runFresh' :: (Typeable i, Enum i, Num i) => Eff (Fresh i :> r) w -> i -> Eff r w
-- > runFresh' m s = fst <$> runState s (loop $ admin m)
-- >  where
-- >   loop (Val x) = return x
-- >   loop (E u)   = case decomp u of
-- >     Right (Fresh k) -> do
-- >                       n <- getState
-- >                       putState (n + 1)
-- >                       loop (k n)
-- >     Left u' -> send (\k -> unsafeReUnion $ k <$> u') >>= loop
module Control.Eff.State(
                        -- * Read-write State
                          State
                        , getState
                        , putState
                        , onState
                        , runState
                        -- * Reader
                        , Reader
                        , getReader
                        , runReader
                        , local
                        -- * Writer
                        , Writer
                        , putWriter
                        , runWriter
                        , runPusher
                        ) where

import Control.Applicative ((<$>), (<|>))
import Data.Typeable

import Control.Eff

-- | Strict state effect
data State s w = State (s -> s) (s -> w)
  deriving (Typeable, Functor)

-- | Write a new value of the state.
putState :: Typeable e => Member (State e) r => e -> Eff r ()
putState = onState . const

-- | Return the current value of the state.
getState :: Typeable e => Member (State e) r => Eff r e
getState = send (inj . State id)

-- | Transform the state with a function.
onState :: (Typeable s, Member (State s) r) => (s -> s) -> Eff r ()
onState f = send (\k -> inj (State f (\_ -> k ())))

-- | Run a State effect.
runState :: Typeable s
         => s                     -- ^ Initial state
         -> Eff (State s :> r) w  -- ^ Effect incorporating State
         -> Eff r (s, w)          -- ^ Effect containing final state and a return value
runState s0 = loop s0 . admin where
 loop s (Val x) = return (s, x)
 loop s (E u)   = handleRelay u (loop s) $
                       \(State t k) -> let s' = t s in s' `seq` loop s' (k s')

-- ------------------------------------------------------------------------
-- The Reader monad

-- | The request for a value of type e from the current environment.
-- This environment is analogous to a parameter of type e.
newtype Reader e v = Reader (e -> v)
    deriving (Typeable, Functor)

-- | Get the current value from a Reader.
getReader :: (Typeable e, Member (Reader e) r) => Eff r e
getReader = send (inj . Reader)

-- | The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w
runReader m e = loop (admin m) where
 loop (Val x) = return x
 loop (E u) = handleRelay u loop (\(Reader k) -> loop (k e))

-- | Locally rebind the value in the dynamic environment.
-- This function both requests and admins Reader requests.
local :: (Typeable e, Member (Reader e) r) =>
     (e -> e) -> Eff r a -> Eff r a
local f m = do
  e <- f <$> getReader
  let loop (Val x) = return x
      loop (E u) = interpose u loop (\(Reader k) -> loop (k e))
  loop (admin m)

-- ------------------------------------------------------------------------
-- | The request to remember a value of type e in the current environment
data Writer e v = Writer e v
    deriving (Typeable, Functor)

putWriter :: (Typeable e, Member (Writer e) r) => e -> Eff r ()
putWriter e = send $ \f -> inj $ Writer e $ f ()

-- | Handle Writer requests by overwriting previous values.
-- If no value of type @e@ was returned, Nothing is returned;
-- otherwise return Just the most recent value written.
runWriter :: Typeable e => Eff (Writer e :> r) w -> Eff r (Maybe e, w)
runWriter = loop . admin
  where
    correctVal f = fmap $ \(x, y) -> (f x, y)

    loop (Val x) = return (Nothing, x)
    loop (E u) = handleRelay u loop (\(Writer e v) -> correctVal (<|> Just e) $ loop v)

-- | Handle Writer requests by stacking written values on to a list.
runPusher :: Typeable e => Eff (Writer e :> r) w -> Eff r ([e], w)
runPusher = loop . admin
  where
    loop (Val x) = return ([], x)
    loop (E u) = handleRelay u loop (\(Writer e v) -> (\(es, v') -> (e:es, v')) <$> loop v)
