module Lib where

import Text.Printf

listAll :: (Bounded a, Enum a) => [a]
listAll = [minBound .. maxBound]

tableDriven :: (Bounded a, Enum a, Eq a, Show a) => [b] -> a -> b
tableDriven = tableDrivenBy listAll

tableDrivenBy :: (Eq a, Show a) => [a] -> [b] -> a -> b
tableDrivenBy xs ys a = maybe outOfBounds id (lookup a (zip xs ys)) where
  outOfBounds = error (printf message (show a))
  message = "Invalid table: No value is assigned to the key \"%s\"."

