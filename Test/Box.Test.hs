{-# LANGUAGE TemplateHaskell #-}

module Box.Test
  where

import Box
import Test.Arbitrary
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

runTests = $quickCheckAll

prop_BoxesWithMoreThanTwoDifferentXesAndYsAreUnequal w x y z =
  (tooMany xs) || (tooMany ys) ==> bound w x /= bound y z
  where
    xs (Point x _) = x
    ys (Point _ y) = y
    tooMany f = threeUnequal (map f [w,x,y,z])
    threeUnequal = (> 2) . length . nub

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

prop_OverlappingBoxesHaveZeroDistance b@(Box (Point x1 y1) (Point x2 y2)) =
  forAll overlappingBoxes $ \b' -> distance b b' == 0
    where
      overlappingBoxes =
        do x <- choose (x1, x2)
           y <- choose (y1, y2)
           xoffset <- choose (x1-x2, x2-x1)
           yoffset <- choose (y1-y2, y2-y1)
           oneof [
            return (bound (Point x y) (Point (x+xoffset) (y+yoffset))),
            return (bound (Point (x+xoffset) (y+yoffset)) (Point x y))
            ]

prop_DistanceIsCommutative a b = distance a b == distance b a
  where types = (a :: Box, b :: Box)

prop_BoxNextToAnotherBoxHasLineToLineDifference =
  distancesOfRotationsAllEqual
    (Box (Point 1 10) (Point 5 20))
    (Box (Point 3 30) (Point 8 33))
    10

distancesOfRotationsAllEqual b1 b2 d =
  all
    (d ==)
    [(distance b1 b2),
     (distance (rotate90 b1) (rotate90 b2)),
     (distance (turn 2 b1) (turn 2 b2)),
     (distance (turn 3 b1) (turn 3 b2))]

prop_MultipleOfFourRotate90sIsTheIdentity p =
 (turn 4 p == p) && (turn 0 p == p)
 where types = (p :: Point)

prop_OneTurnIsOneApplicationOfRotate90 p = rotate90 p == turn 1 p
 where types = (p :: Point)

prop_MultipleOfFourRotate90sForBoxesIsTheIdentity b =
 (turn 4 b == b) && (turn 0 b == b)
 where types = (b :: Box)

prop_OneTurnIsOneApplicationOfRotate90ForBoxes b = rotate90 b == turn 1 b
 where types = (b :: Box)

prop_Rotate90IsOnlyTheIdentityForTheOrigin p =
  (rotate90 p == p) == (p == (Point 0 0))

prop_BoxesThatDoNotShareXOrYValuesAreMeasuredByCornerDistance m@(Box a b@(Point x y))=
  forAll northeasternBoxes $ \n@(Box c d) ->
    distancesOfRotationsAllEqual m n (distance b c)
    where
      offsets i = sized $ \n -> choose (i, i + n)
      northeasternBoxes =
        do xoff <- offsets 1
           yoff <- offsets 1
           w <- offsets 0
           h <- offsets 0
           return $
             bound (Point (x+xoff) (y+yoff)) (Point (x+xoff+w) (y+yoff+h))
