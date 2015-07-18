{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.MetricSpace where

import Data.Profunctor
import qualified Data.Vector as V
import Text.EditDistance

-- | A metric space is a set together with a notion of distance between
-- elements. Distance is computed by a function 'dist' which has the following
-- four laws:
--
--   (1) __non-negative__: @forall x y. 'dist' x y >= 0@
--   (2) __identity of indiscernibles__: @forall x y. 'dist' x y == 0 \<=\> x == y@
--   (3) __symmetry__: @forall x y. dist x y == 'dist' y x@
--   (4) __triangle inequality__: @forall x y z. 'dist' x z <= 'dist' x y + 'dist'1 y z@
newtype MetricSpace a b = MetricSpace { dist :: a -> a -> b }

-- TODO: How to make this useful? Right now the precedence is all wrong.
infixl 8 <->
(<->) :: MetricSpace a b -> a -> a -> b
(<->) = dist

instance Profunctor MetricSpace where
  lmap f (MetricSpace b) = MetricSpace (\x y -> b (f x) (f y))
  {-# INLINE lmap #-}

  rmap f (MetricSpace b) = MetricSpace (\x y -> f (b x y))
  {-# INLINE rmap #-}

-- | Levenshtein distance between 'String's.
levenshtein :: Floating b => MetricSpace String b
levenshtein =
  MetricSpace (\a b -> fromIntegral $ levenshteinDistance defaultEditCosts a b)
{-# INLINE levenshtein #-}

-- | Discrete distance over n-dimensional 'Vector's.
discrete :: (Eq a, Floating b) => MetricSpace (V.Vector a) b
discrete = MetricSpace (\a b -> if a == b then 0 else 1)
{-# INLINE discrete #-}

-- | Euclidean distance over n-dimensional 'Vector's.
-- TODO: Can we be more general here?
euclidean :: MetricSpace (V.Vector Double) Double
euclidean = MetricSpace (\a b -> (f a b) `seq` sqrt (f a b))
  where
    f a b = V.sum (V.zipWith (\x y -> (x-y)^2) a b)
{-# INLINE euclidean #-}
