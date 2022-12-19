{-# language QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Day12 where

import AoCPrelude

import Data.Graph.Inductive.PatriciaTree
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Graph.Inductive
import Data.List (find)

example :: String
example = [__i|
  Sabqponm
  abcryxxl
  accszExk
  acctuvwj
  abdefghi
|]

data HeightMap = MkHM
  { width :: Int
  , height :: Int
  , hm :: IntMap Char
  } deriving (Show, Eq)

type HillGraph = Gr Char Int

task1 :: String -> Maybe Int
task1 = toHeightMap .> shortestPathToE

task2 :: String -> Maybe Int
task2 = toHeightMap .> shortestPathFromEInverse

elevation :: Char -> Int
elevation = \case
  'S' -> 1
  'E' -> 26
  c   -> fromEnum c - 96

shortestPathToE :: HeightMap -> Maybe Int
shortestPathToE heightMap@MkHM{hm} = do
  startNode <- findKey 'S' hm
  endNode <- findKey 'E' hm
  toHillGraph 1 (-26) heightMap
    |> spLength startNode endNode

shortestPathFromEInverse :: HeightMap -> Maybe Int
shortestPathFromEInverse heightMap@MkHM{hm} = do
  startNode <- findKey 'E' hm
  toHillGraph 26 (-1) heightMap
    |> spNodeToValLength startNode 'a'

spNodeToValLength :: Node -> Char -> HillGraph -> Maybe Int
spNodeToValLength start val graph
  =  level start graph
  |> find (\(n, d) -> lab graph n == Just val)
  |> fmap snd

findKey :: Eq a => a -> IntMap a -> Maybe Int
findKey val
  =  IM.toAscList
  .> find (snd .> (== val))
  .> fmap fst

toHeightMap :: String -> HeightMap
toHeightMap
  =  lines
  .> (\cs -> MkHM (length (head cs))
                  (length cs)
                  (toHM (concat cs)))

toHM :: [Char] -> IntMap Char
toHM = zip [0..] .> IM.fromList

toHillGraph :: Int -> Int -> HeightMap -> HillGraph
toHillGraph maxUp maxDown heightMap
  = mkGraph (toNodes heightMap)
            (toEdges maxUp maxDown heightMap)

toNodes :: HeightMap -> [LNode Char]
toNodes = hm .> IM.toAscList

toEdges :: Int -> Int -> HeightMap -> [LEdge Int]
toEdges maxUp maxDown heightMap
  = toNodes heightMap
 |> map fst
 |> concatMap (edgesFromNode maxUp maxDown heightMap)

edgesFromNode :: Int -> Int -> HeightMap -> Node -> [LEdge Int]
edgesFromNode maxUp maxDown heightMap@MkHM{width, height, hm} i
  = to2D width i
 |> adjacent width height
 |> filter (reachableElevation
              maxUp
              maxDown
              (heightAt heightMap i)
              heightMap)
 |> map (from2D width .> (i,,1))

heightAt2D :: HeightMap -> (Int,Int) -> Int
heightAt2D = with2D heightAt

heightAt :: HeightMap -> Int -> Int
heightAt MkHM{hm} i
  = case hm IM.!? i of
      Nothing -> -2
      Just c  -> elevation c  

with2D :: (HeightMap -> Int -> b) -> (HeightMap -> (Int, Int) -> b)
with2D f = \heightMap@MkHM{width, hm} coord
  -> f heightMap (from2D width coord)

reachableElevation :: Int -> Int -> Int -> HeightMap -> (Int,Int) -> Bool
reachableElevation maxUp maxDown el MkHM{width, hm} coord
  = case hm IM.!? from2D width coord of
      Nothing -> False
      Just c  -> betweenOrd maxUp maxDown (elevation c - el)


adjacent :: Int -> Int -> (Int,Int) -> [(Int,Int)]
adjacent w h (x,y)
  = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
 |> filter (\(x',y') -> not (x' < 0  ||
                             y' < 0  || 
                             x' >= w ||
                             y' >= h ))

from2D :: Int -> (Int, Int) -> Int
from2D w (x,y) = x + y*w

to2D :: Int -> Int -> (Int, Int)
to2D w i = (x,y)
  where
    (y,x) = quotRem i w
