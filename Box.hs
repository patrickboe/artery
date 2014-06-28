{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Box
  (
   Point(Point), Box(Box), leftOf, rightOf, above, below, fuse, contains, area, toBox, bound, distance, rotate90, turn, norm
  )
  where

import Set
import Data.List

data Point = Point Int Int
  deriving (Ord, Eq, Show)

data Box = Box Point Point
  deriving (Eq, Show)

instance Set Box Box where
  contains (Box a b) (Box c d) =
    not ((a `rightOf` c) || (a `above` c) || (b `leftOf` d) || (b `below` d))

class Geometric a where
  rotate90 :: a -> a
  distance :: a -> a -> Double

instance Geometric Point where
  rotate90 (Point x y) = (Point y (-x))
  distance (Point x1 y1) (Point x2 y2) =
    sqrt $ fromIntegral $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

fuse (Box (Point x1 y1) (Point x2 y2)) (Box (Point x3 y3) (Point x4 y4)) =
  Box (Point (min x1 x3) (min y1 y3)) (Point (max x2 x4) (max y2 y4))

area (Box (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)

(Point a b) `leftOf` (Point c d) = a < c

(Point a b) `rightOf` (Point c d) = a > c

(Point a b) `above` (Point c d) = b > d

(Point a b) `below` (Point c d) = b < d

bound (Point x1 y1) (Point x2 y2) =
  Box (Point (min x1 x2) (min y1 y2)) (Point (max x1 x2) (max y1 y2))

toBox p = Box p p

getBounds (Box a b) = (a, b)

origin = toBox (Point 0 0)

turn n
  | n == 0 = id
  | otherwise = rotate90 . (turn $ n - 1)

norm unnormalized@(b@(Box (Point x y) _) : others) =
  map n unnormalized where
    n (Box (Point x1 y1) (Point x2 y2)) =
      Box (Point (x1-x) (y1-y)) (Point (x2-x) (y2-y))

data OverlapCategory = Ordered | Reversed | Mixed

instance Geometric Box where

  rotate90 (Box a b) = bound (rotate90 a) (rotate90 b)

  distance s@(Box as bs) t@(Box at bt) =
    let
      xcategory = categorize (extract getx s) (extract getx t)
      ycategory = categorize (extract gety s) (extract gety t)
    in
      case xcategory of
        Ordered -> case ycategory of
                     Ordered -> angleDistance 0
                     Reversed -> angleDistance 3
                     Mixed -> sideDistance 2
        Reversed -> case ycategory of
                      Ordered -> angleDistance 1
                      Reversed -> angleDistance 2
                      Mixed -> sideDistance 0
        Mixed -> case ycategory of
                   Ordered -> sideDistance 3
                   Reversed -> sideDistance 1
                   Mixed -> 0
    where

      extract f (Box a b) = [f a,f b]

      getx (Point x y) = x

      gety (Point x y) = y

      angleDistance rotationsToNE =
        northeastDistance (turn rotationsToNE s) (turn rotationsToNE t)

      sideDistance rotationsToW =
        westDistance (turn rotationsToW s) (turn rotationsToW t)

      westDistance (Box (Point x1 y1) p2) (Box p3 (Point x4 y4)) =
        fromIntegral (x1 - x4)

      northeastDistance (Box a b) (Box c d) =
        distance b c

      categorize l r
        | sorted == l ++ r = Ordered
        | sorted == r ++ l = Reversed
        | otherwise = Mixed
        where
          sorted = sort (l ++ r)
