{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
-- | Coroutines implemented with extensible effects
module Control.Eff.Coroutine( Yield (..)
                            , yield
                            , runC
                            , Y (..)
                            ) where

import Control.Eff1
import Control.Eff.Reader.Strict1
import Data.OpenUnion51
import Control.Eff.Trace

-- ------------------------------------------------------------------------
-- | Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs
--
-- | The yield request: reporting a value of type e and suspending
-- the coroutine. Resuming with the value of type b
data Yield a b v = Yield a (b -> v)

-- | Yield a value of type a and suspend the coroutine.
yield :: (Member (Yield a b) r) => a -> Eff r b
yield x = send (Yield x id)

-- | Status of a thread: done or reporting the value of the type a
--   (For simplicity, a co-routine reports a value but accepts unit)
--
--   Type parameter @r@ is the effect we're yielding from.
--
--   Type parameter @a@ is the type that is yielded.
--
--   Type parameter @w@ is the type of the value returned from the
--   coroutine when it has completed.
data Y r a w = Y a (w -> Eff r (Y r a w))
             | Done


-- | Launch a thread and report its status
runC :: Eff (Yield a b ': r) w -> Eff r (Y r a b)
runC m = handle_relay
  (const $ return Done)
  (\(Yield a f) k -> return $ Y a (k . f))
  m

-- TESTS
-- First example of coroutines
yieldInt :: Member (Yield Int ()) r => Int -> Eff r ()
yieldInt = yield

th1 :: Member (Yield Int ()) r => Eff r ()
th1 = yieldInt 1 >> yieldInt 2


c1 = runTrace (loop =<< runC th1)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop (Done)    = trace ("Done")
{-
1
2
Done
-}

-- Add dynamic variables
-- The code is essentially the same as that in transf.hs (only added
-- a type specializtion on yield). The inferred signature is different though.
-- Before it was
--    th2 :: MonadReader Int m => CoT Int m ()
-- Now it is more general:
th2 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th2 = ask >>= yieldInt >> (ask >>= yieldInt)


-- Code is essentially the same as in transf.hs; no liftIO though
c2 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
Done
-}

-- locally changing the dynamic environment for the suspension
c21 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
Done
-}


-- Real example, with two sorts of local rebinding
th3 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
 where ay = ask >>= yieldInt

c3 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
20
20
Done
-}

-- locally changing the dynamic environment for the suspension
c31 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
21
21
Done
-}
-- The result is exactly as expected and desired: the coroutine shares the
-- dynamic environment with its parent; however, when the environment
-- is locally rebound, it becomes private to coroutine.

-- We now make explicit that the client computation, run by th4,
-- is abstract. We abstract it out of th4
c4 = runTrace $ runReader (loop =<< runC (th4 client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       th4 cl = cl >> local (+(10::Int)) cl
       client = ay >> ay
       ay     = ask >>= yieldInt

{-
10
11
21
21
Done
-}

-- Even more dynamic example
c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (\y->x+1) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= yieldInt

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
10
11
12
18
18
18
29
29
29
29
29
29
Done
-}

-- And even more
c7 = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)

{-
1010
1021
1032
1048
1064
1080
1101
1122
1143
1169
1195
1221
1252
1283
1314
1345
1376
1407
Done
-}

c7' = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Double))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
1010
1021
1032
1048
1048
1048
1069
1090
1111
1137
1137
1137
1168
1199
1230
1261
1292
1323
Done
-}
