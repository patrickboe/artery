{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, FlexibleInstances #-}
module Artery (RTree, Entry(Entry), buildRTree, contains, with, entries, remove, search)
  where

import Control.Monad.Trans.Class
import Control.Monad.Writer.Lazy
import Data.List (foldl1')
import Data.Monoid
import Control.Monad
import Data.Foldable
import Data.Maybe
import CList
import Set
import Box
import Data.Function

data RT a = Leaf Box (CList (Entry a)) | Branch Box (CList (RT a))

mcatMaybes = (fmap fromJust) . (mfilter isJust)

newtype RTree a = RTree (Maybe (RT a))

buildRTree :: [Entry a] -> RTree a
buildRTree = foldl' with $ RTree Nothing

deriving instance Show a => Show (RT a)
deriving instance Show a => Show (RTree a)

data Entry a = Entry Point a

deriving instance Eq a => Eq (Entry a)
deriving instance Ord a => Ord (Entry a)
deriving instance Show a => Show (Entry a)

instance (Eq a) => Set (RTree a) (Entry a) where
  contains t (Entry p x) =
    not $ null $ filter isx $ search t (toBox p)
    where isx (Entry a y) = y == x

data Node n = Node Box (CList n)

class Boxed c a where
  getBox :: (c a) -> Box
  buildTree :: Box -> CList (c a) -> RT a

instance Boxed RT a where
  getBox (Leaf b _) = b
  getBox (Branch b _) = b
  buildTree = Branch

instance Boxed Entry a where
  getBox (Entry p _) = toBox p
  buildTree = Leaf

blocksize = 500

minFirst f xs = foldl' consMin [] xs
  where
    consMin (best:rest) x =
      if f best < f x
      then best:(x:rest)
      else x:(best:rest)
    consMin [] x = [x]

with (RTree (Just t)) e = RTree $ Just $ insert t e
  where
    insert :: (RT a) -> (Entry a) -> (RT a)
    insert (Leaf b es) e =
      expand Leaf (cons e es) b (getBox e)
    insert (Branch b (CList n ts)) e =
      let
        ebox = getBox e
        boundArea b t = area $ fuse (getBox t) b
        st:sts = minFirst (boundArea ebox) ts
        subtrees = CList n $ (insert st e) : sts
      in
        expand Branch subtrees b ebox
with (RTree (Nothing)) e = RTree $ Just $ Leaf (getBox e) (CList 1 [e])

expand ctor ts b box =
  (if count ts <= blocksize
   then ctor $ fuse b box
   else split) ts

split (CList n ns) =
  let (b : (b' : others)) = farthestPairFirst ns
      (l@(Node bl _),r@(Node br _)) =
        foldl'
        splitIter
        (Node (getBox b) (CList 1 [b]), Node (getBox b') (CList 1 [b']))
        others
  in Branch (fuse bl br) (CList 2 [develop l, develop r])
  where develop (Node b xs) = buildTree b xs
        splitIter (x@(Node b ns),x'@(Node b' ns')) n =
          let f = fuse (getBox n) b
              f' = fuse (getBox n) b'
          in
            if area f < area f'
            then (Node f $ (cons n ns), x')
            else (x, Node f' $ (cons n ns'))

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

remove (RTree (Just t)) e@(Entry p v) = RTree $ delete t
  where
    sbox = toBox p
    delete a@(Leaf b es) =
      if b `overlaps` sbox
      then repot $ mfilter (not . (e ==)) es
      else Just a
    delete a@(Branch b ts) =
      if b `overlaps` sbox
      then repot $ mcatMaybes $ fmap delete ts
      else Just a
    repot xs = case xs of
                 (CList 0 []) -> Nothing
                 otherwise -> Just $ buildTree (wrap $ els xs) xs
    wrap :: (Boxed c a) => [c a] -> Box
    wrap = (foldl1' fuse) . (map getBox)

remove (RTree (Nothing)) _ = (RTree (Nothing))

psearch s t =
  case t of
    (Leaf box entries) ->
      forOverlaps box entries $ mfilter $ houses s
    (Branch box trees) ->
      forOverlaps box trees (>>= psearch s)
  where
    forOverlaps b xs f =
      if s `overlaps` b
      then f xs
      else mempty

search (RTree t) s =
  maybe mempty (els . (psearch s)) t

houses b (Entry p x) = b `contains` p

entries :: (RTree a) -> [(Entry a)]
entries (RTree (Just t)) = case ents t of (CList n es) -> es
  where
    ents (Leaf b es) = es
    ents (Branch b ts) = Data.Foldable.foldr1 mappend $ fmap ents ts
entries (RTree (Nothing)) = []
