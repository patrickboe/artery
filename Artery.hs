{-# LANGUAGE StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses #-}

module Artery
  (
   Point(Point), Box(Box), RTree, Entry(Entry), buildRTree,
   contains, bound, getBounds, leftOf, rightOf, above, below, fuse,
   with, entries, remove, find
  )
  where

import Data.List hiding (find)

data Point = Point Int Int
  deriving (Ord, Eq, Show)

data Box = Box Point Point
  deriving (Eq, Show)

data RTree a =
  Leaf Box [Entry a] | Branch Box [RTree a] | MT

buildRTree es = foldl' with MT es

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

data Node n = Node Box [n]

class Boxed c a where
  getBox :: (c a) -> Box
  buildTree :: Box -> [c a] -> RTree a

instance Boxed RTree a where
  getBox (Leaf b _) = b
  getBox (Branch b _) = b
  buildTree = Branch

instance Boxed Entry a where
  getBox (Entry p _) = toBox p
  buildTree = Leaf

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
      if f best < f x
      then best:(x:rest)
      else x:(best:rest)
    consMin [] x = [x]

with :: (RTree a) -> (Entry a) -> (RTree a)
with MT e =
  Leaf (getBox e) [e]
with (Leaf b es) e =
  expand Leaf (e : es) b (getBox e)
with (Branch b ts) e =
  let
    ebox = getBox e
    boundArea b t = area $ fuse (getBox t) b
    st:sts = minFirst (boundArea ebox) ts
    subtrees = (with st e) : sts
  in
    expand Branch subtrees b ebox

expand cons ts b box =
  (if length ts <= blocksize
   then cons $ fuse b box
   else split) ts

split ns =
  let (b : (b' : others)) = farthestPairFirst ns
      (l@(Node bl _),r@(Node br _)) =
        foldl'
        splitIter
        (Node (getBox b) [b], Node (getBox b') [b'])
        others
  in Branch (fuse bl br) [develop l, develop r]
  where farthestPairFirst = id
        develop (Node b xs) = buildTree b xs
        splitIter (x@(Node b ns),x'@(Node b' ns')) n =
          let f = fuse (getBox n) b
              f' = fuse (getBox n) b'
          in
            if area f < area f'
            then (Node f $ n : ns, x')
            else (x, Node f' $ n : ns')

{-
farthestPair (x:xs) =
  foldl swapForFarther (x,x) xs
  where swapForFarther (x,y) z =
          let xyd = boxDist x y
              xzd = boxDist x z
              yzd = boxDist y z
          in
            if xyd > xzd & xyd > yzd
            then (x,y)
            else
              if xzd > xyd & xzd > yzd
              then (x,z)
              else (y,z)
farthestPair [] = (origin,origin)

boxDist b1@(Box p1 p2) b2@(Box p3 p4) =
  if b1 `overlaps` b2
  then 0
  else minimum $ zip edgeDist [rightEdge b1,leftEdge b1]) [leftEdge b2,rightEdge b2]

rightEdge :: Box -> Edge
leftEdge b = Edge (Point 0 0) (Point 0 0)

leftEdge :: Box -> Edge
leftEdge b = Edge (Point 0 0) (Point 0 0)

edgeDist :: Edge -> Edge -> Int
edgeDist e1 e2 = 0
-}

remove :: (RTree a) -> (Entry a) -> (RTree a)
remove t e = t

find :: (RTree a) -> Box -> [(Entry a)]
find t b = []

entries :: (RTree a) -> [(Entry a)]
entries MT = []
entries (Leaf b es) = es
entries (Branch b ts) = foldr1 (++) $ map entries ts
