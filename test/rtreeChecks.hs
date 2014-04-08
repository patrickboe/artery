import Test.QuickCheck

newtype Num a => BBox a = (a,a)

data RTree a = Point a | Branch BBox (RTree a) (RTree a)

instance Arbitrary BBox where
  arbitrary = (arbitrary a, arbitrary a)
