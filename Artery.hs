{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module Artery
  (
   Point(Point),
   Box(Box),
   RTree(MT),
   Entry(Entry),
   contains,
   bound,
   getBounds,
   leftOf,
   rightOf,
   above,
   below,
   fuse,
   insert,
   entries,
   remove,
   find
  )
  where

data Point = Point Int Int
  deriving (Ord, Eq, Show)

data Box = Box Point Point
  deriving (Eq, Show)

data RTree a = MT | Leaf (Entry a) | Branch Box (RTree a) (RTree a)

deriving instance Show a => Show (RTree a)

data Entry a = Entry Point a

deriving instance Eq a => Eq (Entry a)
deriving instance Ord a => Ord (Entry a)
deriving instance Show a => Show (Entry a)

contains (Box a b) (Box c d) =
  not ((a `rightOf` c) || (a `above` c) || (b `leftOf` d) || (b `below` d))

fuse (Box (Point x1 y1) (Point x2 y2)) (Box (Point x3 y3) (Point x4 y4)) =
  Box (Point (min x1 x3) (min y1 y3)) (Point (max x2 x4) (max y2 y4))

area (Box (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)

(Point a b) `leftOf` (Point c d) = a < c

(Point a b) `rightOf` (Point c d) = a > c

(Point a b) `above` (Point c d) = b > d

(Point a b) `below` (Point c d) = b < d

bound (Point x1 y1) (Point x2 y2) =
  Box (Point (min x1 x2) (min y1 y2)) (Point (max x1 x2) (max y1 y2))

getBounds (Box a b) = (a, b)

origin = (bound (Point 0 0) (Point 0 0))

insert :: (RTree a) -> (Entry a) -> (RTree a)
insert MT e         = Leaf e
insert l@(Leaf _) e = Branch origin l (Leaf e)
insert x e          = Branch origin x (Leaf e)

remove :: (RTree a) -> (Entry a) -> (RTree a)
remove t e = t

find :: (RTree a) -> Box -> [(Entry a)]
find t b = []

entries :: (RTree a) -> [(Entry a)]
entries MT = []
entries (Leaf e) = [e]
entries (Branch b s1 s2) = entries s1 ++ entries s2
