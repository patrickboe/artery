import CList
import Data.List
import Data.Foldable
import Test.QuickCheck
import Control.Monad
import Criterion.Main

cvector :: Arbitrary a => Int -> Gen (CList a)
cvector n = liftM (CList n) (vector n)

main =
  do cxs <- generate (cvector 100000)
     xs <- generate (vector 100000)
     let
         sumCList :: Int -> Int
         sumCList = ((Data.Foldable.foldl1 (+)) . (flip ctake cxs))
         sumCList' :: Int -> Int
         sumCList' = ((Data.Foldable.foldl' (+) 0) . (flip ctake cxs))
         sumList :: Int -> Int
         sumList = ((Data.List.foldl1 (+)) . (flip take xs))
         sumList' :: Int -> Int
         sumList' = ((Data.List.foldl' (+) 0) . (flip take xs))
         fsumList :: Int -> Int
         fsumList = ((Data.Foldable.foldl1 (+)) . (flip take xs))
         fsumList' :: Int -> Int
         fsumList' = ((Data.Foldable.foldl' (+) 0) . (flip take xs))
         countCList = count . (flip ctake cxs)
         countList = length . (flip take xs)
     defaultMain
       [ bgroup "Folds"
         [ bench "Sum a List" $ nf sumList 10000
         , bench "Sum a List using Foldable" $ nf fsumList 10000
         , bench "Strict Sum a List" $ nf sumList' 10000
         , bench "Strict Sum a List using Foldable" $ nf fsumList' 10000
         , bench "Sum a CList" $ nf sumCList 10000
         , bench "Strict Sum a CList" $ nf sumCList' 10000
         ]
       , bgroup "Quantify"
         [ bench "List Length" $ nf countList 10000
         , bench "CList Count" $ nf countCList 10000
         ]
       ]
