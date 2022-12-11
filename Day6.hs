module Day6 where
import Data.List.Extra (anySame)
import AoCPrelude

example :: String
example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

task1 :: String -> Int
task1 signal = go signal 0
  where
    go (a:b:c:d:rest) i
      | allDifferent [a,b,c,d] = i + 4
      | otherwise = go (b:c:d:rest) (i + 1)

allDifferent :: Eq a => [a] -> Bool
allDifferent = not . anySame

task2 :: String -> Int
task2 signal = go signal 0
  where
    go str i
      | allDifferent (take 14 str) = i + 14
      | otherwise = go (tail str) (i+1)