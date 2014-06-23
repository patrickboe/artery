{-# LANGUAGE TemplateHaskell #-}

module Artery.Test
  where

import Artery
import Box
import Test.Arbitrary
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List hiding (find)
import qualified Data.Set as Set

runTests = $quickCheckAll

toSet = Set.fromList . entries

indices xs = take (length xs) (iterate (1 +) 0)

cut i xs = (take i xs) ++ (drop (i + 1) xs)

shufflesOf [] = return []
shufflesOf xs = do i <- oneof $ map return $ indices xs
                   s <- shufflesOf $ cut i xs
                   return $ xs !! i : s

sameEntrySets (tree, entries) = toSet tree == Set.fromList entries

removeFromBoth e (tree, xs) = (tree `remove` e, delete e xs)

addToBoth e (tree, xs) = (tree `with` e, e : xs)

newtype Act a = Act ((RTree a,[Entry a]) -> (RTree a,[Entry a]))

instance Show a => Show (Act a) where
  show x = "(some Act)"

instance (Arbitrary a,Ord a) => Arbitrary (Act a) where
  arbitrary = oneof $ map toChangeGen [addToBoth,removeFromBoth]
    where toChangeGen f = liftM (Act . f) arbitrary

instance Arbitrary a => Arbitrary (Entry a) where
  arbitrary = arb2 Entry

instance Arbitrary a => Arbitrary (RTree a) where
  arbitrary = do e <- arbitrary
                 es <- arbitrary
                 return $ buildRTree $ e : es

subsetsOf = Set.foldr maybeWith (return Set.empty) where
  maybeWith x xsg =
      do xs <- xsg
         oneof [return (Set.insert x xs), return xs]

prop_InsertAugmentsComputedSet es rt =
  let rt' = foldl' with rt es
  in toSet rt' == toSet rt `Set.union` Set.fromList es
  where types = (es :: [Entry Int],rt :: RTree Int)

ignore_prop_ATreeContainsExactlyTheSetOfInsertedElements es e =
  let rt = buildRTree es
  in (all (contains rt) es) && ((rt `contains` e) == (e `elem` es))

ignore_prop_RemoveDiminishesComputedSet es rt =
  forAll (subsetsOf $ toSet rt) $ \sub ->
    let rt' = Set.foldl' remove rt sub
    in toSet rt' == toSet rt `Set.difference` sub

ignore_prop_FindIncludesAllEntriesInSearchBox b rt =
  (Set.fromList $ find rt b) == (Set.filter inBox $ toSet rt)
  where inBox (Entry p x) = b `contains` (Box p p)

ignore_prop_AnyRemovalOrderProducesAConsistentSeriesOfEntrySets es =
  forAll (shufflesOf es) $ \removals ->
    all sameEntrySets $ scanl (flip removeFromBoth) ((buildRTree es),es) removals

ignore_prop_AnySequenceOfInsertionsAndRemovalsProducesConsistentEntrySets es acts =
  all sameEntrySets $ scanl run ((buildRTree es),es) acts
  where run tuple (Act f) = f tuple

{-
  -- todo:
  -- search intersection
  -- nearest
  -- nearestk
-}
