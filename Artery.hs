module Artery
  (
   Point(Point),
   Box(Box),
   RTree(Leaf, Branch),
   Entry(Entry),
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

data Box = Box Point Point
  deriving (Eq, Show)

data RTree = Leaf Box Int | Branch Box RTree RTree
  deriving (Show)

data Entry = Entry Point Int

contains (Box a b) (Box c d) =
  not ((a `rightOf` c) || (a `above` c) || (b `leftOf` d) || (b `below` d))

(Point a b) `leftOf` (Point c d) = a < c

(Point a b) `rightOf` (Point c d) = a > c

(Point a b) `above` (Point c d) = b > d

(Point a b) `below` (Point c d) = b < d

bound (Point x1 y1) (Point x2 y2) =
  Box (Point (min x1 x2) (min y1 y2)) (Point (max x1 x2) (max y1 y2))

getBounds (Box a b) = (a, b)

insert :: Entry -> RTree -> RTree
insert e t = t

remove :: Entry -> RTree -> RTree
remove e t = t

find :: Box -> RTree -> [Entry]
find b t = []
