module Artery.Test
  where

import Artery
import Control.Monad
import Test.QuickCheck

instance Arbitrary Point where
  arbitrary = liftM2 Point arbitrary arbitrary

instance Arbitrary BBox where
  arbitrary = liftM2 BBox arbitrary arbitrary

instance Arbitrary RTree where
  arbitrary = sized rtree'
    where rtree' 0 = liftM2 Leaf arbitrary arbitrary
          rtree' n = oneof [liftM2 Leaf arbitrary arbitrary,
                      liftM3 Branch arbitrary subtree subtree]
            where subtree = rtree' (n `div` 2)

prop_EqualBBoxes x = x `contains` x
  where types = x::BBox
