{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Math.MetricSpace
-- Copyright   :  (C) 2015 Ricky Elrod, Tony Morris
-- License     :  BSD2 (see LICENSE file)
--
-- Maintainer  :  Ricky Elrod <ricky@elrod.me>
-- Stability   :  provisional
-- Portability :  lens
--
-- A 'MetricSpace' is a set together with a notion of distance between
-- elements. Distance is computed by a function 'dist' which has the following
-- four laws:
--
--   (1) __non-negative__: @forall x y. 'dist' x y >= 0@
--   (2) __identity of indiscernibles__: @forall x y. 'dist' x y == 0 \<=\> x == y@
--   (3) __symmetry__: @forall x y. dist x y == 'dist' y x@
--   (4) __triangle inequality__: @forall x y z. 'dist' x z <= 'dist' x y + 'dist' y z@
--
-- See the Wikipedia <https://en.wikipedia.org/wiki/Metric_space article on metric spaces>
-- for more details.
----------------------------------------------------------------------------
module Math.MetricSpace where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Lens
import Data.Function (on)
import Data.Functor.Contravariant.Divisible
import Data.Profunctor
import Data.Semigroup
import Data.Semigroupoid
import qualified Data.Vector as V
import Text.EditDistance

-- $setup
-- >>> import qualified Data.Vector as V

newtype MetricSpace a b = MetricSpace { dist :: a -> a -> b }

type ClosedMetricSpace a = MetricSpace a a
newtype FlippedMetricSpace b a = FlippedMetricSpace (MetricSpace a b)
type ClosedFlippedMetricSpace a = FlippedMetricSpace a a

-- TODO: How to make this useful? Right now the precedence is all wrong.
infixl 8 <->
(<->) :: MetricSpace a b -> a -> a -> b
(<->) = dist
{-# INLINE (<->) #-}

instance Functor (MetricSpace a) where
  fmap = dimap id
  {-# INLINE fmap #-}

instance Applicative (MetricSpace a) where
  pure = MetricSpace . const . const
  {-# INLINE pure #-}
  MetricSpace f <*> MetricSpace a = MetricSpace (\x y -> f x y (a x y))
  {-# INLINE (<*>) #-}

instance Monad (MetricSpace a) where
  return = pure
  {-# INLINE return #-}
  MetricSpace f >>= fn =
    MetricSpace (\x y -> let MetricSpace s =  fn (f x y) in s x y)
  {-# INLINE (>>=) #-}

instance Semigroup b => Semigroup (MetricSpace a b) where
  MetricSpace m1 <> MetricSpace m2 =
    MetricSpace (\a1 a2 -> m1 a1 a2 <> m2 a1 a2)
  {-# INLINE (<>) #-}

instance Monoid b => Monoid (MetricSpace a b) where
  mempty = MetricSpace . const . const $ mempty
  {-# INLINE mempty #-}
  mappend (MetricSpace m1) (MetricSpace m2) =
    MetricSpace (\a1 a2 -> m1 a1 a2 `mappend` m2 a1 a2)
  {-# INLINE mappend #-}

instance Profunctor MetricSpace where
  lmap f (MetricSpace b) = MetricSpace (b `on` f)
  {-# INLINE lmap #-}

  rmap f (MetricSpace b) = MetricSpace (\x y -> f (b x y))
  {-# INLINE rmap #-}

instance Semigroupoid MetricSpace where
  MetricSpace m1 `o` MetricSpace m2 =
    MetricSpace (\a1 a2 -> let b = m2 a1 a2 in m1 b b)
  {-# INLINE o #-}

_FlippedMetricSpace
  :: Iso
     (MetricSpace a b)
     (MetricSpace x y)
     (FlippedMetricSpace b a)
     (FlippedMetricSpace y x)
_FlippedMetricSpace = iso FlippedMetricSpace (\(FlippedMetricSpace m) -> m)
{-# INLINE _FlippedMetricSpace #-}

instance Contravariant (FlippedMetricSpace b) where
  contramap f (FlippedMetricSpace m) = FlippedMetricSpace (dimap f id m)
  {-# INLINE contramap #-}

instance Monoid b => Divisible (FlippedMetricSpace b) where
  divide
    f
    (FlippedMetricSpace (MetricSpace m1))
    (FlippedMetricSpace (MetricSpace m2)) =
      FlippedMetricSpace (MetricSpace (\a1 a2 -> let (b1, c1) = f a1
                                                     (b2, c2) = f a2
                                                 in
                                                   m1 b1 b2 `mappend` m2 c1 c2))
  {-# INLINE divide #-}
  conquer = FlippedMetricSpace . MetricSpace . const . const $ mempty
  {-# INLINE conquer #-}

class SwappedMetricSpace m where
  _SwappedMetricSpace :: Iso (m a b) (m x y) (m a b) (m x y)

instance SwappedMetricSpace MetricSpace where
  _SwappedMetricSpace =
    iso
      (\(MetricSpace m) -> MetricSpace (flip m))
      (\(MetricSpace m) -> MetricSpace (flip m))
  {-# INLINE _SwappedMetricSpace #-}

instance SwappedMetricSpace FlippedMetricSpace where
  _SwappedMetricSpace =
    iso
      (\(FlippedMetricSpace (MetricSpace m)) ->
         FlippedMetricSpace (MetricSpace (flip m)))
      (\(FlippedMetricSpace (MetricSpace m)) ->
         FlippedMetricSpace (MetricSpace (flip m)))
  {-# INLINE _SwappedMetricSpace #-}

-- | Levenshtein distance between 'String's.
--
-- >>> dist levenshtein "foo" "bar"
-- 3
--
-- >>> dist levenshtein "hi" "ha"
-- 1
--
-- >>> dist levenshtein "ff" "ff"
-- 0
levenshtein :: Integral b => MetricSpace String b
levenshtein =
  MetricSpace (\a b -> fromIntegral $ levenshteinDistance defaultEditCosts a b)
{-# INLINE levenshtein #-}

-- | Discrete distance over n-dimensional 'Vector's.
--
-- >>> dist discrete (V.fromList [3,4]) (V.fromList [3,4])
-- 0
--
-- >>> dist discrete (V.fromList [1,49]) (V.fromList [3,-94])
-- 1
discrete :: (Eq a, Integral b) => MetricSpace (V.Vector a) b
discrete = MetricSpace (\a b -> if a == b then 0 else 1)
{-# INLINE discrete #-}

-- | Euclidean distance over n-dimensional 'Vector's.
--
-- >>> dist euclidean (V.fromList [3,4]) (V.fromList [3,4])
-- 0.0
--
-- >>> dist euclidean (V.fromList [1,49]) (V.fromList [3,-94])
-- 143.01398533010678
euclidean :: RealFloat a => MetricSpace (V.Vector a) a
euclidean = MetricSpace (\a b -> f a b `seq` sqrt (f a b))
  where
    f a b = V.sum (V.zipWith (\x y -> (x-y)^2) a b)
{-# INLINE euclidean #-}

-- | Taxicab distance over n-dimensional 'Vector's.
--
-- >>> dist taxicab (V.fromList [3,4]) (V.fromList [3,4])
-- 0.0
--
-- >>> dist taxicab (V.fromList [1,49]) (V.fromList [3,-94])
-- 145.0
taxicab :: RealFloat a => MetricSpace (V.Vector a) a
taxicab = MetricSpace f
  where
    f a b = V.sum (V.zipWith (\x y -> abs (x-y)) a b)
{-# INLINE taxicab #-}

-- | Hamming distance over n-dimensional 'Vector's.
hamming :: (Eq a, Integral b) => MetricSpace (V.Vector a) b
hamming =
  MetricSpace (\x y ->
                 fromIntegral .
                 V.length .
                 V.filter (uncurry (/=)) $ V.zip x y)
{-# INLINE hamming #-}
