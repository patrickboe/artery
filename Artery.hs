{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, FlexibleInstances #-}
module Artery (RTree, Entry(Entry), buildRTree, contains, with, entries, remove, find)
  where

import Data.List hiding (find)
import Set
import Box
import Data.Function

data RTree a =
  Leaf Box [Entry a] | Branch Box [RTree a]

buildRTree (e : es) = foldl' with (Leaf (getBox e) [e]) es

deriving instance Show a => Show (RTree a)

data Entry a = Entry Point a

deriving instance Eq a => Eq (Entry a)
deriving instance Ord a => Ord (Entry a)
deriving instance Show a => Show (Entry a)

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

blocksize = 4

minFirst f xs = foldl' consMin [] xs
  where
    consMin (best:rest) x =
      if f best < f x
      then best:(x:rest)
      else x:(best:rest)
    consMin [] x = [x]

with :: (RTree a) -> (Entry a) -> (RTree a)
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
  {- TODO: cut out this unnecessary count -}
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
  where develop (Node b xs) = buildTree b xs
        splitIter (x@(Node b ns),x'@(Node b' ns')) n =
          let f = fuse (getBox n) b
              f' = fuse (getBox n) b'
          in
            if area f < area f'
            then (Node f $ n : ns, x')
            else (x, Node f' $ n : ns')

farthestPairFirst (x : (y : xs)) =
  foldl' swapForFarther (x : (y : [])) xs
  where swapForFarther (x : (y : xs)) z =
          let xyd = nodeDist x y
              xzd = nodeDist x z
              yzd = nodeDist y z
          in
            if (xyd > xzd) && (xyd > yzd)
            then (x : (y : (z : xs)))
            else
              if (xzd > xyd) && (xzd > yzd)
              then (x : (z : (y : xs)))
              else (y : (z : (x : xs)))
          where
            nodeDist = distance `on` getBox
farthestPairFirst xs = xs

remove :: (RTree a) -> (Entry a) -> (RTree a)
remove t e = t

find :: (RTree a) -> Box -> [(Entry a)]
find t b = []

entries :: (RTree a) -> [(Entry a)]
entries (Leaf b es) = es
entries (Branch b ts) = foldr1 (++) $ map entries ts
