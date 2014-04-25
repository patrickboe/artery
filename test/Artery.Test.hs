module Artery.Test
  where

import Artery
import Control.Monad
import Test.QuickCheck

instance Arbitrary Point where
  arbitrary = liftM2 Point arbitrary arbitrary
