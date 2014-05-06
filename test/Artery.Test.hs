{-# LANGUAGE TemplateHaskell #-}

module Artery.Test
  where

import Artery
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List hiding (insert,find)
import qualified Data.Set as Set

runTests = $quickCheckAll

toSet = Set.fromList . entries

arb2 x = liftM2 x arbitrary arbitrary

subsetsOf = Set.foldr maybeWith (return Set.empty)
  where x `maybeWith` xsg =
          do xs <- xsg
             oneof [return (Set.insert x xs), return xs]

instance Arbitrary Point where
  arbitrary = arb2 Point

instance Arbitrary Box where
  arbitrary = arb2 bound

instance Arbitrary a => Arbitrary (Entry a) where
  arbitrary = arb2 Entry

instance Arbitrary a => Arbitrary (RTree a) where
  arbitrary = liftM (foldl' insert MT) arbitrary

prop_BoxesWithAnUncommonPointAreUnequal w x y z =
  threeUnequal [w,x,y,z] ==> bound w x /= bound y z
    where threeUnequal = (> 2) . length . nub

prop_EqualBoxesContainEachOther b = b `contains` b
  where types = [b :: Box]

prop_ContainingBoxSurroundsHorizontally x@(Box a b) y@(Box c d) =
  (a `leftOf` c) && (b `leftOf` d) ==> not (x `contains` y)

prop_ContainingBoxSurroundsVertically x@(Box a b) y@(Box c d) =
  (a `above` c) && (b `above` d) ==> not (x `contains` y)

prop_ABoxWithBordersWithinAnotherBoxIsContained b =
  forAll (innerBoxesOf b) $ contains b
    where innerBoxesOf (Box (Point x1 y1) (Point x2 y2)) =
            do (x3,x4) <- chooseTwo x1 x2
               (y3,y4) <- chooseTwo y1 y2
               return (bound (Point x3 y3) (Point x4 y4))
          chooseTwo a b = liftM2 (,) (choose (a,b)) (choose (a,b))

prop_BoundsListBottomLeftPointFirst (Box p1 p2) =
  not ((p1 `rightOf` p2) || (p1 `above` p2))

prop_ConstructedBoundsAreSameAsThoseOfTheOriginalPoints a@(Point x1 y1) b@(Point x2 y2) =
  case (bound a b) of
    (Box (Point x3 y3) (Point x4 y4)) ->
      (max x1 x2) == (max x3 x4) && (max y1 y2) == (max y3 y4) &&
      (min x1 x2) == (min x3 x4) && (min y1 y2) == (min y3 y4)

prop_FuseProducesABoxContainingThePreviousTwoBoxes b1 b2 =
  let papa = fuse b1 b2 in
      papa `contains` b1 && papa `contains` b2

prop_InsertAugmentsComputedSet es rt =
  let rt' = foldl' insert rt es
  in toSet rt' == toSet rt `Set.union` Set.fromList es

prop_RemoveDiminishesComputedSet es rt =
  forAll (subsetsOf $ toSet rt) $ \sub ->
    let rt' = Set.foldl' remove rt sub
    in toSet rt' == toSet rt `Set.difference` sub

prop_FindIncludesAllEntriesInSearchBox b rt =
  (Set.fromList $ find rt b) == (Set.filter inBox $ toSet rt)
  where inBox (Entry p x) = b `contains` (Box p p)
