{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Math.MetricSpace where

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
class MetricSpace a where
  dist :: a -> a -> Double

  infix 0 <->
  -- | Infix notation for 'dist'.
  (<->) :: a -> a -> Double
  (<->) = dist

-- | Euclidean distance
data EuclideanVector a where
  EuclideanVector :: Floating a => V.Vector a -> EuclideanVector a

-- | Discrete distance
newtype DiscreteVector a = DiscreteVector (V.Vector a) deriving (Eq)

-- | Levenshtein distance
newtype Levenshtein = Levenshtein String

-- | Levenshtein distance between 'String's.
instance MetricSpace Levenshtein where
  dist (Levenshtein a) (Levenshtein b) =
    fromIntegral $ levenshteinDistance defaultEditCosts a b

-- | Discrete distance over n-dimensional 'Vector's.
instance Eq a => MetricSpace (DiscreteVector a) where
  dist a b = if a == b then 0 else 1

-- | Euclidean distance over n-dimensional 'Vector's.
-- TODO: I want this to be way more general.
instance MetricSpace (EuclideanVector Double) where
  dist (EuclideanVector a) (EuclideanVector b) =  f `seq` sqrt f
    where f = V.sum (V.zipWith (\x y -> (x-y)^2) a b)
