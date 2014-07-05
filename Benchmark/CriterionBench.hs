import Criterion.Main
import Test.QuickCheck
import Box
import Artery

size = 1000000

samplePoint =
  do x <- choose (-5000,5000)
     y <- choose (-5000,5000)
     return $ Point x y

samplePoints = vectorOf size samplePoint

sampleEntries =
  do ps <- samplePoints
     return $ zipWith Entry ps [1..]

main =
  do es <- generate sampleEntries
     let buildSample n = buildRTree $ take n es
     defaultMain
       [
         bgroup "buildRTree" [ bench "1000" $ whnf buildSample 1000
                             , bench "10000" $ whnf buildSample 10000
                             , bench "1000000" $ whnf buildSample 1000000
                             ]
       ]
