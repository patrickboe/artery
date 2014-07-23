{-# LANGUAGE StandaloneDeriving #-}
module CList (CList(CList), count, cons, els, dcons, ctake, cdrop)
 where

import Data.Monoid
import Control.Monad
import Data.Foldable

data CList a = CList { count :: Int, els :: [a] }

deriving instance Show a => Show (CList a)
deriving instance Eq a => Eq (CList a)

instance Functor CList where
  fmap f (CList n xs) = CList n $ fmap f xs

instance Monad CList where
  return x = CList 1 [x]
  CList 0 [] >>= f = CList 0 []
  CList n (x:xs) >>= f = f x `mappend` (CList (n - 1) xs >>= f)

instance Monoid (CList a) where
  mappend (CList n1 xs) (CList n2 ys) = CList (n1 + n2) (xs ++ ys)
  mempty = CList 0 []

instance MonadPlus CList where
  mzero = mempty
  mplus = mappend

instance Foldable CList where
  foldMap f (CList n xs) = foldMap f xs

cons x (CList n xs) = CList (n + 1) (x : xs)

dcons (CList n (x : xs)) = (x, (CList (n - 1) xs))

ctake n (CList m xs) = CList n (take n xs)

cdrop n (CList m xs) = CList (m - n) (drop n xs)
