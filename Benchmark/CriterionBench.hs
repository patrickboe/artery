import Control.Monad
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

execTrace rt b =
  execWriterT $ talliedSearch rt b

trace =
  do bs <- generate sampleBoxes
     fes <- generate sampleEntries
     let
       rt = buildRTree $ take 10000 fes
       loop =
         do
           putStrLn "which box to get?"
           x <- getLine
           putStrLn $ "searching in " ++ x
           print $ execTrace rt $ bs !! (read x)
           loop
     loop

warm rt = length $ search rt (bound (Point 0 0) (Point 0 0))

main =
  do es <- generate sampleEntries
     bs <- generate sampleBoxes
     fes <- generate sampleEntries
     let buildSample n = warm $ buildRTree $ take n es
         rt1000 = buildRTree $ take 1000 fes
         rt10000 = buildRTree $ take 10000 fes
         rt100000 = buildRTree $ take 100000 fes
         warm1 = warm rt1000
         warm2 = warm rt10000
         warm3 = warm rt100000
         warm4 = warm rt1000000
         runSampleSearches rt = sum . (map $ length . (search rt)) $ take 1000 bs
     defaultMain
       [
         bgroup "buildRTree"
           [ bench "with 1000 entries" $ nf buildSample 1000
           , bench "with 10000 entries" $ nf buildSample 10000
           , bench "with 100000 entries" $ nf buildSample 100000
           , bench "with 1000000 entries" $ nf buildSample 1000000
           ]
       , warm1 `seq` warm2 `seq` warm3 `seq` warm4 `seq`
           bgroup "perform 1000 random searches"
             [ bench "in a 1000 node tree" $ nf runSampleSearches rt1000
             , bench "in a 10000 node tree" $ nf runSampleSearches rt10000
             , bench "in a 100000 node tree" $ nf runSampleSearches rt100000
             , bench "in a 1000000 node tree" $ nf runSampleSearches rt1000000
             ]
       ]
