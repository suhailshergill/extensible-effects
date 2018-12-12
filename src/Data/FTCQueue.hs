{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

-- | Fast type-aligned queue optimized to effectful functions
-- (a -> m b)
-- (monad continuations have this type). TODO: FIXME
-- Constant-time append and snoc and
-- average constant-time left-edge deconstruction
module Data.FTCQueue (
  FTCQueue,
  tsingleton,
  (|>), -- snoc
  (><), -- append
  ViewL(..),
  viewlMap,
  tviewl
  )
  where

-- | Non-empty tree. Deconstruction operations make it more and more
-- left-leaning. Common values of 'm' are 'Arr (Eff r)' and 'AsUnitLoop a'.
data FTCQueue m a b where
  Leaf :: m a b -> FTCQueue m a b
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b


-- Exported operations

-- | There is no tempty: use (tsingleton return), which works just the same.
-- The names are chosen for compatibility with FastTCQueue
{-# INLINE tsingleton #-}
tsingleton :: m a b -> FTCQueue m a b
tsingleton r = Leaf r

-- | snoc: clearly constant-time
{-# INLINE (|>) #-}
(|>) :: FTCQueue m a x -> m x b -> FTCQueue m a b
t |> r = Node t (Leaf r)

-- | append: clearly constant-time
{-# INLINE (><) #-}
(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
t1 >< t2 = Node t1 t2


-- | Left-edge deconstruction
data ViewL m a b where
  TOne  :: m a b -> ViewL m a b
  (:|)  :: m a x -> (FTCQueue m x b) -> ViewL m a b

-- | Process the Left-edge deconstruction
{-# INLINE viewlMap #-}
viewlMap :: ViewL m a b
         -> (m a b -> c)
         -> (forall x. m a x -> (FTCQueue m x b) -> c)
         -> c
viewlMap view tone cons = case view of
  TOne k -> tone k
  k :| t -> cons k t

{-# INLINABLE tviewl #-}
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
 where
   go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
   go (Leaf r) tr = r :| tr
   go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)
