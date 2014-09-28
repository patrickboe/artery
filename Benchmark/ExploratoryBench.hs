import Control.Monad
import Control.DeepSeq
import Control.Monad.Writer.Lazy
import Criterion.Main
import Test.QuickCheck
import Box
import Artery

size = 1000000
radius = 10

samplePoint = liftM2 Point (choose (-5000,5000)) (choose (-5000,5000))

sampleBox r =
  do (Point x y) <- samplePoint
     return $ Box (Point (x - r) (y - r)) (Point (x + r) (y + r))

sampleBoxes = infiniteListOf $ sampleBox radius

samplePoints = infiniteListOf samplePoint

sampleEntries =
  do ps <- samplePoints
     return $ zipWith Entry ps [1..]

sampleTree = liftM buildRTree sampleEntries

main =
  do es <- generate sampleEntries
     bs <- generate sampleBoxes
     fes <- generate sampleEntries
     let buildSample n = buildRTree $ take n es
         rt100 = buildRTree $ take 100 fes
         rt1000 = buildRTree $ take 1000 fes
         rt10000 = buildRTree $ take 10000 fes
         rt100000 = buildRTree $ take 100000 fes
         rt1000000 = buildRTree $ take 1000000 fes
         runSampleSearches rt = sum . (map $ length . (search rt)) $ take 1bs
     defaultMain
       [ rt100 `deepseq`
           bgroup "perform 1 random search"
             [ bench "in a 100 node tree" $ nf runSampleSearches rt100]
       ]
