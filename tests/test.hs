{-# LANGUAGE ScopedTypeVariables #-}
import Data.Profunctor
import qualified Data.Vector as V
import Math.MetricSpace
import Test.Tasty
import Test.Tasty.QuickCheck as QC

data SameLengthVector3 a = SameLengthVector3 (V.Vector a) (V.Vector a) (V.Vector a) deriving Show

main = defaultMain $ opts $ tests
  where
    opts = localOption (QuickCheckTests 3000)

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

nonnegative :: (Num b, Ord b) => MetricSpace a b -> a -> a -> Bool
nonnegative m a b = dist m a b >= 0

indisc :: (Eq b, Num b) => MetricSpace a b -> a -> Bool
indisc m a = dist m a a == 0

symmetry :: Eq b => MetricSpace a b -> a -> a -> Bool
symmetry m a b = dist m a b == dist m b a

triangle :: (Num b, Ord b) => MetricSpace a b -> a -> a -> a -> Bool
triangle m a b c = (dist m a c) <= (dist m a b) + (dist m b c)

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary

triangleVect
  :: (Num b, Ord b, Show a, Arbitrary a) =>
     MetricSpace (V.Vector a) b -> V.Vector a -> Property
triangleVect m xs =
  forAll (vectorOf (V.length xs) arbitrary) $ \ys ->
  forAll (vectorOf (V.length xs) arbitrary) $ \zs -> triangle m xs (V.fromList ys) (V.fromList zs)

genTestGroup
  :: (Num b, Ord b, Show a, Arbitrary a, Eq a) =>
     TestName -> MetricSpace a b -> TestTree
genTestGroup name metric =
  testGroup name [
        QC.testProperty "nonnegative" $ \a b -> nonnegative metric a b
      , QC.testProperty "indisc" $ \a -> indisc metric a
      , QC.testProperty "symmetry" $ \a b -> symmetry metric a b
      , QC.testProperty "triangle" $ \a b -> triangle metric a b
      , QC.testProperty "profunctor lmap" $ \a b -> dist (lmap id metric) a b == dist metric a b
      , QC.testProperty "profunctor rmap" $ \a b -> dist (rmap id metric) a b == dist metric a b
    ]

genTestGroupV :: TestName -> MetricSpace (V.Vector Double) Double -> TestTree
genTestGroupV name metric =
  testGroup name [
        QC.testProperty "nonnegative" $ \a b -> nonnegative metric a b
      , QC.testProperty "indisc" $ \a -> indisc metric a
      , QC.testProperty "symmetry" $ \a b -> symmetry metric a b
      , QC.testProperty "triangle" $ triangleVect metric
      , QC.testProperty "profunctor lmap" $ \a b -> dist (lmap id metric) a b == dist metric a b
      , QC.testProperty "profunctor rmap" $ \a b -> dist (rmap id metric) a b == dist metric a b
    ]

qcProps = testGroup "(checked by QuickCheck)"
  [
    genTestGroup "levenshtein" levenshtein
  --, genTestGroup "discrete" discrete
  , genTestGroupV "euclidean" euclidean
  , genTestGroupV "taxicab" taxicab
  ]
