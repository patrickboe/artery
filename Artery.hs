{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, FlexibleInstances #-}
module Artery (RTree, Entry(Entry), buildRTree, contains, with, entries, remove, search, talliedSearch)
  where

import Control.Monad.Trans.Class
import Control.Monad.Writer.Lazy
import Data.Monoid
import Control.Monad
import Data.List
import Data.Maybe
import Set
import Box
import Data.Function

data RT a =
  Leaf Box [Entry a] | Branch Box [RT a]

newtype RTree a = RTree (Maybe (RT a))

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

data Node n = Node Box [n]

class Boxed c a where
  getBox :: (c a) -> Box
  buildTree :: Box -> [c a] -> RT a

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
      expand Leaf (e : es) b (getBox e)
    insert (Branch b ts) e =
      let
        ebox = getBox e
        boundArea b t = area $ fuse (getBox t) b
        st:sts = minFirst (boundArea ebox) ts
        subtrees = (insert st e) : sts
      in
        expand Branch subtrees b ebox
with (RTree (Nothing)) e = RTree $ Just $ Leaf (getBox e) [e]

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

remove (RTree (Just t)) e = RTree $ delete t e
  where
    delete a@(Leaf b es) e =
      let sbox = getBox e
      in
        if b `overlaps` sbox
        then prune $ filter (not . (sbox `houses`)) es
        else Just a
    delete a@(Branch b ts) s =
      if b `overlaps` (getBox s)
      then prune $ catMaybes $ map (flip delete s) ts
      else Just a
    prune [] = Nothing
    prune xs = Just $ buildTree (wrap xs) xs
    wrap :: (Boxed c a) => [c a] -> Box
    wrap = (foldl1' fuse) . (map getBox)

remove (RTree (Nothing)) _ = (RTree (Nothing))

type SearchResultsWithOverlapTally a = WriterT (Sum Int) [] (Entry a)

psearch :: Box -> RT a -> SearchResultsWithOverlapTally a
psearch s t =
  case t of
    (Leaf box entries) ->
      forOverlaps box entries $ mfilter $ houses s
    (Branch box trees) ->
      forOverlaps box trees (>>= psearch s)
  where
    forOverlaps b xs f =
      if s `overlaps` b
      then do tell (Sum 1)
              (f . lift) xs
      else mzero

talliedSearch (RTree t) s =
  maybe mzero (psearch s) t

search (RTree t) s =
  maybe mzero ((map fst) . (runWriterT . psearch s)) t

houses b (Entry p x) = b `contains` p

entries :: (RTree a) -> [(Entry a)]
entries (RTree (Just t)) = ents t
  where
    ents (Leaf b es) = es
    ents (Branch b ts) = foldr1 (++) $ map ents ts
entries (RTree (Nothing)) = []
