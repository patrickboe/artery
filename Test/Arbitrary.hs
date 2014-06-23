{-# LANGUAGE TemplateHaskell #-}

module Test.Arbitrary
  where

import Box
import Control.Monad
import Test.QuickCheck

arb2 x = liftM2 x arbitrary arbitrary

instance Arbitrary Point where
  arbitrary = arb2 Point

instance Arbitrary Box where
  arbitrary = arb2 bound
