{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Box
  (
   Point(Point), Box(Box), leftOf, rightOf, above, below, fuse, contains, area, toBox, bound, distance, rotate90
  )
  where

import Set

data Point = Point Int Int
  deriving (Ord, Eq, Show)

data Box = Box Point Point
  deriving (Eq, Show)

instance Set Box Box where
  contains (Box a b) (Box c d) =
    not ((a `rightOf` c) || (a `above` c) || (b `leftOf` d) || (b `below` d))

class Geometric a where
  distance :: Floating b => a -> a -> b

instance Geometric Point where
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

instance Geometric Box where
  distance a b = 0

rotate90 axis@(Point x' y') subject =
  let (Point x y) = norm subject
  in denorm (Point y (-x))
  where
    norm (Point x y) = (Point (x - x') (y - y'))
    denorm (Point x y) = (Point (x + x') (y + y'))
