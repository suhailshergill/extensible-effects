{-# LANGUAGE GADTs #-}
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

-- type FastTCQueue =  ToCatQueue FastQueue
-- instance TASequence FastQueue where
--  tempty = RQ CNil SNil CNil
--  tsingleton x = let c = tsingleton x in queue c SNil c
--  (RQ f r a) |> x = queue f (r `Snoc` x) a

--  tviewl (RQ CNil SNil CNil) = TAEmptyL
--  tviewl (RQ (h `Cons` t) f a) = h :< queue t f a

--  tmap phi (RQ a b c) = RQ (tmap phi a) (tmap phi b) (tmap phi c)
--
-- data ToCatQueue q c x y where
--   C0 :: ToCatQueue q c x x
--   CN :: c x y -> !(q (ToCatQueue q c) y z) -> ToCatQueue q c x z

-- instance TASequence q => TASequence (ToCatQueue q) where
--  tempty       = C0
--  tsingleton a = CN a tempty
--  C0        >< ys  = ys
--  xs        >< C0  = xs
--  (CN x q)  >< ys  = CN x (q |> ys)

--  tviewl C0        = TAEmptyL
--  tviewl (CN h t)  = h :< linkAll t
--    where
--     linkAll :: TASequence q =>  q (ToCatQueue q c) a b -> ToCatQueue q c a b
--     linkAll v = case tviewl v of
--      TAEmptyL     -> C0
--      CN x q :< t  -> CN x (q `snoc` linkAll t)
--     snoc q C0  = q
--     snoc q r   = q |> r

--  tmap phi C0 = C0
--  tmap phi (CN c q) = CN (phi c) (tmap (tmap phi) q)

-- class TASequence s where

--   tempty     :: s c x x
--   tsingleton :: c x y -> s c x y
--   -- | Append two type aligned sequences
--   (><)       :: s c x y -> s c y z  -> s c x z
--   -- | View a type aligned sequence from the left
--   tviewl     :: s c x y -> TAViewL s c x y
--   -- | View a type aligned sequence from the right
--   --
--   -- Default definition:
--   --
--   -- > tviewr q = case tviewl q of
--   -- >   TAEmptyL -> TAEmptyR
--   -- >   h :< t -> case tviewr t of
--   -- >        TAEmptyR -> tempty   :> h
--   -- >        p :> l   -> (h <| p) :> l
--   tviewr     :: s c x y -> TAViewR s c x y
--   -- | Append a single element to the right
--   --
--   -- Default definition:
--   --
--   -- > l |> r = l >< tsingleton r

--   (|>)       :: s c x y -> c y z -> s c x z
--   -- | Append a single element to the left
--   --
--   -- Default definition:
--   --
--   -- > l <| r = tsingleton l >< r

--   (<|)       :: c x y -> s c y z -> s c x z
--   -- | Apply a function to all elements in a type aligned sequence
--   --
--   -- Default definition:
--   --
--   -- > tmap f q = case tviewl q of
--   -- >    TAEmptyL -> tempty
--   -- >    h :< t -> f h <| tmap f t
--   tmap       :: (forall x y. c x y -> d x y) -> s c x y -> s d x y

--   l |> r = l >< tsingleton r
--   l <| r = tsingleton l >< r
--   l >< r = case tviewl l of
--     TAEmptyL -> r
--     h :< t  -> h <| (t >< r)

--   tviewl q = case tviewr q of
--     TAEmptyR -> TAEmptyL
--     p :> l -> case tviewl p of
--         TAEmptyL -> l :< tempty
--         h :< t   -> h :< (t |> l)

--   tviewr q = case tviewl q of
--     TAEmptyL -> TAEmptyR
--     h :< t -> case tviewr t of
--         TAEmptyR -> tempty   :> h
--         p :> l   -> (h <| p) :> l

--   tmap f q = case tviewl q of
--     TAEmptyL -> tempty
--     h :< t -> f h <| tmap f t


-- | Non-empty tree. Deconstruction operations make it more and more
-- left-leaning
-- FIXME: don't export constructor
newtype FTCQueue m a b = FTCQueue (TA.FTCQ.FastTCQueue (Kleisli m) a b)
-- type FTCQueue m a b = TA.FTCQ.FastTCQueue (Kleisli m) a b

-- data FTCQueue m a b where
--   Leaf :: (a -> m b) -> FTCQueue m a b
--   Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b


-- Exported operations

-- | There is no tempty: use (tsingleton return), which works just the same.
-- The names are chosen for compatibility with FastTCQueue
{-# INLINE tsingleton #-}
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = FTCQueue . TA.tsingleton . Kleisli
-- tsingleton r = Leaf r

-- | snoc: clearly constant-time
{-# INLINE (|>) #-}
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
(FTCQueue t) |> r = FTCQueue $ t TA.|> (Kleisli r)

-- | append: clearly constant-time
{-# INLINE (><) #-}
(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(FTCQueue t1) >< (FTCQueue t2) = FTCQueue $ t1 TA.>< t2


-- | Left-edge deconstruction
-- FIXME: export function to map over TOne and (:|), but not constructors of ViewL
newtype ViewL m a b = ViewL (TA.TAViewL TA.FTCQ.FastTCQueue (Kleisli m) a b)
-- data ViewL m a b where
--   TOne  :: (a -> m b) -> ViewL m a b
--   (:|)  :: (a -> m x) -> (FTCQueue m x b) -> ViewL m a b

{-# INLINABLE viewlMap #-}
viewlMap :: ((Kleisli m) a b -> c) -- ^ TOne
         -> (c -> (FTCQueue m a b) -> c) -- ^ (:|)
         -> ViewL x a b -> c
viewlMap tone cons (ViewL tav) = case tav of
  TA.TAEmptyL -> error "FTCQueue: the impossible happened!"
  k TA.:< t1 -> case t1 of
    TA.TAEmptyL -> tone k
    k TA.:< t2 -> (tone k) `cons` t2

{-# INLINABLE tviewl #-}
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (FTCQueue t1) = ViewL $ TA.tviewl t1
-- tviewl (Leaf r) = TOne r
-- tviewl (Node t1 t2) = go t1 t2
--  where
--    go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
--    go (Leaf r) tr = r :| tr
--    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)
