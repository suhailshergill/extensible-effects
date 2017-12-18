{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

-- | Fast type-aligned queue optimized to effectful functions
-- (a -> m b)
-- (monad continuations have this type).
-- Constant-time append and snoc and
-- average constant-time left-edge deconstruction
module Data.FTCQueue (
  FTCQueue,
  tsingleton,
  (|>), -- snoc
  (><), -- append
  ViewL,
  viewlMap,
  tviewl
  )
  where

import qualified Data.TASequence.FastCatQueue as TA.FTCQ
import qualified Data.TASequence as TA
import Control.Arrow (Kleisli (..))

-- | Non-empty tree. Deconstruction operations make it more and more
-- left-leaning
newtype FTCQueue m a b = FTCQueue (TA.FTCQ.FastTCQueue (Kleisli m) a b)

-- | Left-edge deconstruction
newtype ViewL m a b = ViewL (TA.TAViewL TA.FTCQ.FastTCQueue (Kleisli m) a b)


-- Exported operations

-- | There is no tempty: use (tsingleton return), which works just the same.
-- The names are chosen for compatibility with FastTCQueue
{-# INLINE tsingleton #-}
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = FTCQueue . TA.tsingleton . Kleisli

-- | snoc: clearly constant-time
{-# INLINE (|>) #-}
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
(FTCQueue t) |> r = FTCQueue $ t TA.|> (Kleisli r)

-- | append: clearly constant-time
{-# INLINE (><) #-}
(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(FTCQueue t1) >< (FTCQueue t2) = FTCQueue $ t1 TA.>< t2

-- | Process the Left-edge deconstruction
{-# INLINE viewlMap #-}
viewlMap :: ViewL m a b
         -> ((a -> m b) -> c)
         -> (forall x. (a -> m x) -> (FTCQueue m x b) -> c)
         -> c
viewlMap (ViewL tav) tone cons = case tav of
  TA.TAEmptyL -> error "Impossibility: FTCQueue constructor allowed to create empty sequence"
  (Kleisli k) TA.:< t1 -> case TA.tviewl t1 of
    TA.TAEmptyL -> tone k
    _ -> k `cons` (FTCQueue t1)

{-# INLINABLE tviewl #-}
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (FTCQueue t1) = ViewL $ TA.tviewl t1
