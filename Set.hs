{-# LANGUAGE MultiParamTypeClasses #-}

module Set ( Set, contains ) where

class (Eq e) => Set s e where
  contains :: s -> e -> Bool
