module Artery
  where

data Point = Point Int Int
  deriving (Eq, Show)

data BBox = BBox Point Point
  deriving (Eq, Show)

data RTree = Leaf BBox Int | Branch BBox RTree RTree
  deriving (Show)

contains :: BBox -> BBox -> Bool
contains = (==)
