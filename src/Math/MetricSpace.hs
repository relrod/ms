{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Math.MetricSpace where

-- | A metric space is a set together with a notion of distance between
-- elements. Distance is computed by a function 'dist' which has the following
-- four laws:
--
--   (1) __non-negative__: @forall x y. 'dist' x y >= 0@
--   (2) __identity of indiscernibles__: @forall x y. 'dist' x y == 0 \<=\> x == y@
--   (3) __symmetry__: @forall x y. dist x y == 'dist' y x@
--   (4) __triangle inequality__: @forall x y z. 'dist' x z <= 'dist' x y + 'dist'1 y z@
class MetricSpace a where
  dist :: Floating b => a -> a -> b

  infix 0 <->
  -- | Infix notation for 'dist'.
  (<->) :: Floating b => a -> a -> b
  (<->) = dist

newtype Euclidean1 a = Euclidean1 { getEuclidean1 :: Integral a => a }
newtype Euclidean2 a b =
  Euclidean2 { getEuclidean2 :: (Integral a, Integral b) => (a, b) }
newtype Euclidean3 a b c =
  Euclidean3 {
    getEuclidean3 :: (Integral a, Integral b, Integral c) => (a, b, c) }
newtype Euclidean4 a b c d =
  Euclidean4 {
    getEuclidean4 ::
      (Integral a, Integral b, Integral c, Integral d) => (a, b, c, d) }
newtype Euclidean5 a b c d e =
  Euclidean5 {
    getEuclidean5 ::
      (Integral a, Integral b, Integral c, Integral d, Integral e) =>
      (a, b, c, d, e) }

newtype Discrete1 a = Discrete1 { getDiscrete1 :: a } deriving (Eq)
newtype Discrete2 a b =
  Discrete2 { getDiscrete2 :: (a, b) } deriving (Eq)
newtype Discrete3 a b c =
  Discrete3 {
    getDiscrete3 :: (a, b, c) } deriving (Eq)
newtype Discrete4 a b c d =
  Discrete4 {
    getDiscrete4 :: (a, b, c, d) } deriving (Eq)
newtype Discrete5 a b c d e =
  Discrete5 {
    getDiscrete5 :: (a, b, c, d, e) } deriving (Eq)

-- | 1-dimensional discrete distance.
instance (Eq a) => MetricSpace (Discrete1 a) where
  dist a b = if a == b then 0 else 1

-- | 2-dimensional discrete distance.
instance (Eq a, Eq b) => MetricSpace (Discrete2 a b) where
  dist a b = if a == b then 0 else 1

-- | 3-dimensional discrete distance.
instance (Eq a, Eq b, Eq c) => MetricSpace (Discrete3 a b c) where
  dist a b = if a == b then 0 else 1

-- | 4-dimensional discrete distance.
instance (Eq a, Eq b, Eq c, Eq d) => MetricSpace (Discrete4 a b c d) where
  dist a b = if a == b then 0 else 1

-- | 5-dimensional discrete distance.
instance (Eq a, Eq b, Eq c, Eq d, Eq e) =>
         MetricSpace (Discrete5 a b c d e) where
  dist a b = if a == b then 0 else 1

-- | 1-dimensional euclidean distance.
instance Integral a => MetricSpace (Euclidean1 a) where
  dist (Euclidean1 a) (Euclidean1 b) =
    sqrt ((fromIntegral a - fromIntegral b)^2)

-- | 2-dimensional euclidean distance.
instance (Integral a, Integral b) => MetricSpace (Euclidean2 a b) where
  dist (Euclidean2 (a, b)) (Euclidean2 (c, d)) =
    sqrt (
      (fromIntegral a - fromIntegral c)^2 +
      (fromIntegral b - fromIntegral d)^2)

-- | 3-dimensional euclidean distance.
instance (Integral a, Integral b, Integral c) => MetricSpace (Euclidean3 a b c) where
  dist (Euclidean3 (a, b, c)) (Euclidean3 (d, e, f)) =
    sqrt (
      (fromIntegral a - fromIntegral d)^2 +
      (fromIntegral b - fromIntegral e)^2 +
      (fromIntegral c - fromIntegral f)^2)

-- | 4-dimensional euclidean distance.
instance (
  Integral a,
  Integral b,
  Integral c,
  Integral d) => MetricSpace (Euclidean4 a b c d) where
  dist (Euclidean4 (a, b, c, d)) (Euclidean4 (e, f, g, h)) =
    sqrt (
      (fromIntegral a - fromIntegral e)^2 +
      (fromIntegral b - fromIntegral f)^2 +
      (fromIntegral c - fromIntegral g)^2 +
      (fromIntegral d - fromIntegral h)^2)

-- | 5-dimensional euclidean distance.
instance (
  Integral a,
  Integral b,
  Integral c,
  Integral d,
  Integral e) => MetricSpace (a, b, c, d, e) where
  dist (a, b, c, d, e) (f, g, h, i, j) =
    sqrt (
      (fromIntegral a - fromIntegral f)^2 +
      (fromIntegral b - fromIntegral g)^2 +
      (fromIntegral c - fromIntegral h)^2 +
      (fromIntegral d - fromIntegral i)^2 +
      (fromIntegral e - fromIntegral j)^2)
