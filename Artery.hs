{-# LANGUAGE StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses #-}

module Artery
  (
   Point(Point), Box(Box), RTree, Entry(Entry),
   contains, bound, getBounds, leftOf, rightOf, above, below, fuse,
   insert, entries, remove, find
  )
  where

data Point = Point Int Int
  deriving (Ord, Eq, Show)

data Box = Box Point Point
  deriving (Eq, Show)

data RTree a = Leaf Box [Entry a] | Branch Box [RTree a]

deriving instance Show a => Show (RTree a)

data Entry a = Entry Point a

deriving instance Eq a => Eq (Entry a)
deriving instance Ord a => Ord (Entry a)
deriving instance Show a => Show (Entry a)

class (Eq e) => Set s e where
  contains :: s -> e -> Bool

instance Set Box Box where
  contains (Box a b) (Box c d) =
    not ((a `rightOf` c) || (a `above` c) || (b `leftOf` d) || (b `below` d))

instance (Eq a) => Set (RTree a) (Entry a) where
  contains t x = True

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

blocksize = 4

minFirst f xs = foldl' consMin [] xs
where
  consMin (best:rest) x =
    if f best < f c
    then best:(x:rest)
    else x:(best:rest)
  consMin [] x = [x]

{-TODO: think about abstracting a 'node' for less repetition around bounding boxes -}
insert :: (RTree a) -> (Entry a) -> (RTree a)
insert t e = case (broaden t e) of
               (x, None) -> x
               (x, Just y) -> Branch 
insert (Leaf b es) e@(Entry p x) =
 (if length es < blocksize
  then Leaf $ b `fuse` $ toBox p
  else split-leaf) $ e : es
insert (Branch b ts) e@(Entry p x) =
  let
    boundArea p (Leaf b _) = area $ fuse b $ toBox p
    boundArea p (Branch b _) = area $ fuse b $ toBox p
    st:sts = minFirst (boundArea p) ts
    subtrees = (insert st e) ++ sts
  in
  (if length subtrees <= blocksize
   then Branch $ b `fuse` $ toBox p
   else split-branch) subtrees

remove :: (RTree a) -> (Entry a) -> (RTree a)
remove t e = t

find :: (RTree a) -> Box -> [(Entry a)]
find t b = []

entries :: (RTree a) -> [(Entry a)]
entries (Leaf b es) = es
entries (Branch b ts) = foldr1 (++) map entries ts
