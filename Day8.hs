{-# language QuasiQuotes #-}
module Day8 where

import AoCPrelude

import Data.List

import Data.Vector (Vector)
import Data.Vector qualified as V

example :: String
example = [__i|
  30373
  25512
  65332
  33549
  35390
|]

type Vec2D a = Vector (Vector a)

type TreeGrid = Grid Int

type Grid a = [[a]]

toVec2D :: String -> Vec2D Int
toVec2D = lines
       .> map (map (\x -> read [x] :: Int) .> V.fromList)
       .> V.fromList

toTreeGrid :: String -> TreeGrid
toTreeGrid = lines .> map (map (\x -> read [x] :: Int))

task1 :: String -> Int
task1 = toTreeGrid
     .> visibilities
     .> concat
     .> filter id
     .> length

visibleTree :: [Int] -> [Bool]
visibleTree [] = []
visibleTree (t0:ts0) = True : go t0 ts0
  where
    go _ [] = []
    go maxSoFar (t:ts)
      | t > maxSoFar = True : go t ts
      | otherwise    = False : go maxSoFar ts

-- Feels like there's probably an existing abstraction for this, probably some kind of optics?
visibilityWithInvertibleTrans :: (Grid Int -> [[Int]]) -> ([[Bool]] -> [[Bool]]) -> TreeGrid -> [[Bool]]
visibilityWithInvertibleTrans trans trans' = trans .> map visibleTree .> trans'

visibilities :: TreeGrid -> [[Bool]]
visibilities tg = merge (merge west east) (merge north south)
  where
    west = visibilityWithInvertibleTrans id id tg
    east = visibilityWithInvertibleTrans (map reverse) (map reverse) tg
    north = visibilityWithInvertibleTrans transpose transpose tg
    south = visibilityWithInvertibleTrans (transpose .> map reverse) (map reverse .> transpose) tg

merge :: [[Bool]] -> [[Bool]] -> [[Bool]]
merge = zipWith (zipWith (||))

-- Task 2 (not the most intuitive solution, but this requires an annoying access pattern)

-- I'm sure there's a better way
splits :: [a] -> [([a],a,[a])]
splits xs = map (\i -> case splitAt i xs of (ls, r:rs) -> (ls, r, rs)) [0..length xs - 1] 

task2 :: String -> Int
task2 = toTreeGrid
     .> (\tg -> map splits tg `combineSplits` map splits (transpose tg))
     .> maximum

combineSplits :: [[([Int], Int, [Int])]] -> [[([Int], Int, [Int])]] -> [Int]
combineSplits horizonts verts = zipWith f (concat horizonts) (concat (transpose verts))
  where
    f :: ([Int], Int, [Int]) -> ([Int], Int, [Int]) -> Int
    f (westR, x, east) (northR, y, south)
      | x /= y = error "Focus not equal"
      | otherwise = [north, west, east, south]
                 |> map (visibleDistance x)
                 |> product 
      where
        west = reverse westR
        north = reverse northR

visibleDistance :: Int -> [Int] -> Int
visibleDistance x = takeUntil1 (x <=) .> length

takeUntil1 :: (a -> Bool) -> [a] -> [a]
takeUntil1 p [] = []
takeUntil1 p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil1 p xs