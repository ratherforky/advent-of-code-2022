{-# language QuasiQuotes #-}
module Day3 where

import AoCPrelude
import Data.Char
import Data.List.Extra

example :: String
example = [__i|
  vJrwpWtwJgWrhcsFMMfFFhFp
  jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
  PmmdzqPrVvPwwTWBwg
  wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
  ttgJtRGJQctTZtZT
  CrZsJsPPZsGzwwsLwLmpwMDw
|]

task1 :: String -> Int
task1
  = lines
  .> map (splitInHalf
          .> uncurry intersect
          .> head
          .> priority)
  .> sum

splitInHalf :: [a] -> ([a],[a])
splitInHalf xs = splitAt (length xs `div` 2) xs

priority :: Char -> Int
priority c
  | isUpper c = fromEnum c - 38
  | isLower c = fromEnum c - 96
  | otherwise = error "Not an alphabetical character"

task2 :: String -> Int
task2
  = lines
  .> chunksOf 3
  .> map (foldr1 intersect .> head .> priority)
  .> sum
