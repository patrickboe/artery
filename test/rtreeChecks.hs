import Test.QuickCheck
import Control.Monad

data Point = Point Int Int

data BBox = BBox Point Point

data RTree = Leaf BBox Int | Branch BBox RTree RTree

prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

instance Arbitrary Point where
  arbitrary = do x <- choose (0,10)
                 y <- choose (0,10)
                 return (Point x y)
