module Day1 where

import Data.List
import Flow
import Data.Ord

testInput :: String
testInput = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

getInput :: IO String
getInput = readFile "Day1-Input.txt"

task1 :: String -> Int
task1
  =  lines
  .> splitElvesGroup
  .> convertToInt
  .> map sum
  .> maximum

splitElvesGroup :: [String] -> [[String]]
splitElvesGroup xs = filter p (groupBy f xs)
  where
    f :: String -> String -> Bool
    f "" _ = False
    f _ "" = False
    f _ _ = True

    p :: [String] -> Bool
    p [""] = False
    p _ = True

stringToInt :: String -> Int
stringToInt xs = read xs

convertToInt :: [[String]] -> [[Int]]
convertToInt = map (map stringToInt)
  -- where
  --   f :: [String] -> [Int]
  --   f = (map stringToInt)

runTask1 :: IO ()
runTask1 = do
  input <- getInput
  print (task1 input)

---------------------------------------
-- Task 2
---------------------------------------

task2 :: String -> Int
task2
  =  lines
  .> splitElvesGroup
  .> convertToInt
  .> map sum
  .> sortOn Down
  .> take 3
  .> sum

runTask2 :: IO ()
runTask2 = do
  input <- getInput
  print (task2 input)
