import CList
import Data.List
import Data.Foldable
import Test.QuickCheck
import Control.Monad
import Criterion.Main

cvector :: Arbitrary a => Int -> Gen (CList a)
cvector n = liftM (CList n) (vector n)

xs :: [Int]
xs = [1..100000]

cs :: CList Int
cs = CList 100000 xs

sumList f = f (+) xs

sumCList f = f (+) cs

main =
  do defaultMain
       [ bgroup "Folds"
         [ bench "Sum a List" $ nf sumList Data.List.foldl1
         , bench "Sum a Foldable" $ nf sumList Data.Foldable.foldl1
         , bench "Strict Sum a List" $ nf sumList (flip Data.List.foldl' 0)
         , bench "Strict Sum a Foldable" $ nf sumList (flip Data.Foldable.foldl' 0)
         , bench "Sum a CList" $ nf sumCList Data.Foldable.foldl1
         , bench "Strict Sum a CList" $ nf sumCList (flip Data.Foldable.foldl' 0)
         ]
       , bgroup "Quantify"
         [ bench "List Length" $ nf length xs
         , bench "CList Length" $ nf count cs
         ]
       ]
