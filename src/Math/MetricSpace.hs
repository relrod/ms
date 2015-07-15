{-# LANGUAGE RankNTypes #-}

module Math.MetricSpace where

-- | A metric space is a set together with a notion of distance between
-- elements. Distance is computed by a function 'dist' which has the following
-- four laws:
--
--   (1) __non-negative__: @forall x y. 'dist' x y >= 0@
--   (2) __identity of indiscernibles__: @forall x y. 'dist' x y == 0 <=> x == y@
--   (3) __symmetry__: @forall x y. dist x y == 'dist' y x@
--   (4) __triangle inequality__: @forall x y z. 'dist' x z <= 'dist' x y + 'dist' y z@
class MetricSpace a where
  dist :: Floating b => a -> a -> b

  infix 0 <->
  -- | Infix notation for 'dist'.
  (<->) :: Floating b => a -> a -> b
  (<->) = dist

newtype WrappedIntegral a = WrappedIntegral { getA :: Integral a => a }

-- | 1-dimensional euclidean distance.
instance Integral a => MetricSpace (WrappedIntegral a) where
  dist (WrappedIntegral a) (WrappedIntegral b) =
    sqrt ((fromIntegral a - fromIntegral b)^2)

-- | 2-dimensional euclidean distance.
instance (Integral a, Integral b) => MetricSpace (a, b) where
  dist (a, b) (c, d) =
    sqrt (
      (fromIntegral a - fromIntegral c)^2 +
      (fromIntegral b - fromIntegral d)^2)

-- | 3-dimensional euclidean distance.
instance (Integral a, Integral b, Integral c) => MetricSpace (a, b, c) where
  dist (a, b, c) (d, e, f) =
    sqrt (
      (fromIntegral a - fromIntegral d)^2 +
      (fromIntegral b - fromIntegral e)^2 +
      (fromIntegral c - fromIntegral f)^2)

-- | 4-dimensional euclidean distance.
instance (
  Integral a,
  Integral b,
  Integral c,
  Integral d) => MetricSpace (a, b, c, d) where
  dist (a, b, c, d) (e, f, g, h) =
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
