module Math.MetricSpace where

import Data.Function (on)
import Data.Profunctor
import Data.Semigroup
import qualified Data.Vector as V
import Text.EditDistance

-- $setup
-- >>> import qualified Data.Vector as V

-- | A metric space is a set together with a notion of distance between
-- elements. Distance is computed by a function 'dist' which has the following
-- four laws:
--
--   (1) __non-negative__: @forall x y. 'dist' x y >= 0@
--   (2) __identity of indiscernibles__: @forall x y. 'dist' x y == 0 \<=\> x == y@
--   (3) __symmetry__: @forall x y. dist x y == 'dist' y x@
--   (4) __triangle inequality__: @forall x y z. 'dist' x z <= 'dist' x y + 'dist' y z@
newtype MetricSpace a b = MetricSpace { dist :: a -> a -> b }

-- TODO: How to make this useful? Right now the precedence is all wrong.
infixl 8 <->
(<->) :: MetricSpace a b -> a -> a -> b
(<->) = dist

instance Functor (MetricSpace a) where
  fmap = dimap id

instance Applicative (MetricSpace a) where
  pure = MetricSpace . const . const
  MetricSpace f <*> MetricSpace a = MetricSpace (\x y -> f x y (a x y))

instance Monad (MetricSpace a) where
  return = pure
  MetricSpace f >>= fn =
    MetricSpace (\x y -> let MetricSpace s =  fn (f x y) in s x y)

instance Semigroup b => Semigroup (MetricSpace a b) where
  MetricSpace m1 <> MetricSpace m2 =
    MetricSpace (\a1 a2 -> m1 a1 a2 <> m2 a1 a2)

instance Monoid b => Monoid (MetricSpace a b) where
  mempty = MetricSpace . const . const $ mempty
  mappend (MetricSpace m1) (MetricSpace m2) =
    MetricSpace (\a1 a2 -> m1 a1 a2 `mappend` m2 a1 a2)

instance Profunctor MetricSpace where
  lmap f (MetricSpace b) = MetricSpace (b `on` f)
  {-# INLINE lmap #-}

  rmap f (MetricSpace b) = MetricSpace (\x y -> f (b x y))
  {-# INLINE rmap #-}

-- | Levenshtein distance between 'String's.
--
-- >>> dist levenshtein "foo" "bar"
-- 3.0
--
-- >>> dist levenshtein "hi" "ha"
-- 1.0
--
-- >>> dist levenshtein "ff" "ff"
-- 0.0
levenshtein :: Floating b => MetricSpace String b
levenshtein =
  MetricSpace (\a b -> fromIntegral $ levenshteinDistance defaultEditCosts a b)
{-# INLINE levenshtein #-}

-- | Discrete distance over n-dimensional 'Vector's.
--
-- >>> dist discrete (V.fromList [3,4]) (V.fromList [3,4])
-- 0.0
--
-- >>> dist discrete (V.fromList [1,49]) (V.fromList [3,-94])
-- 1.0
discrete :: (Eq a, Floating b) => MetricSpace (V.Vector a) b
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
