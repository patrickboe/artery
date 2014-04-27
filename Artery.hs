module Artery
  (
   Point(Point),
   BBox,
   RTree(Leaf, Branch),
   contains,
   bound,
   getBounds,
   leftOf,
   rightOf,
   above,
   below
  )
  where

data Point = Point Int Int
  deriving (Eq, Show)

data BBox = BBox Point Point
  deriving (Eq, Show)

data RTree = Leaf BBox Int | Branch BBox RTree RTree
  deriving (Show)

contains x y = x == y

(Point a b) `leftOf` (Point c d) = a < c

(Point a b) `rightOf` (Point c d) = a > c

(Point a b) `above` (Point c d) = b > d

(Point a b) `below` (Point c d) = b < d

bound (Point x1 y1) (Point x2 y2) =
  BBox (Point (min x1 x2) (min y1 y2)) (Point (max x1 x2) (max y1 y2))

getBounds (BBox a b) = (a, b)
