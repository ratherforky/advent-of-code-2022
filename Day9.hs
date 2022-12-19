{-# language QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Day9 where

import AoCPrelude
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative

data CMD = MkCMD Dir Int
  deriving (Show, Eq)

data Dir
  = R
  | L
  | U
  | D
  deriving (Show, Eq)

example :: String
example = [__i|
  R 4
  U 4
  L 3
  D 1
  R 4
  D 1
  L 5
  R 2
|]

inputParser :: Parser [CMD]
inputParser = everyLine (liftA2 MkCMD dir int)

dir :: Parser Dir
dir = choice
  [ R <$ chunk "R " 
  , L <$ chunk "L "
  , U <$ chunk "U "
  , D <$ chunk "D "
  ]

data StateHT = MkS
  { ropePos :: [Coord]
  , tailVisited :: Set Coord
  } deriving (Show, Eq)

type Coord = (Int, Int)

start :: Int -> StateHT
start n = MkS (replicate n (0,0)) (S.singleton (0,0))

moveD :: Dir -> Coord -> Coord
moveD = \case
  L -> move (-1) 0
  R -> move 1 0
  U -> move 0 1
  D -> move 0 (-1)


move :: Int -> Int -> Coord -> Coord
move dx dy (x,y) = (x+dx,y+dy)

cmdSem :: CMD -> StateHT -> StateHT
cmdSem cmd (MkS rope visited) = MkS{..}
  where
    (ropePos, tailCoords) = moveRope cmd rope
    tailVisited = S.union tailCoords visited

moveRope :: CMD -> [Coord] -> ([Coord], Set Coord)
moveRope (MkCMD d n) rope = (last intermediateRopes, tailCoordSet)
  where
    intermediateRopes = applyNScan n go rope
    tailCoordSet = S.fromList $ map last intermediateRopes

    go [] = []
    go (coord0:coords) = catchupAll (coord0':coords)
      where
        coord0' = moveD d coord0

catchupAll :: [Coord] -> [Coord]
catchupAll [] = []
catchupAll (coord0:coord1:coords) = coord0 : catchupAll (coord1':coords)
  where
    coord1'
      | touching coord0 coord1 = coord1
      | otherwise = catchup (coord0, coord1)
catchupAll [coord] = [coord]

touching :: Coord -> Coord -> Bool
touching (x0,y0) (x1,y1)
  = within 1 x0 x1 && within 1 y0 y1

catchup :: (Coord,Coord) -> Coord
catchup ((x0,y0), (x1,y1)) = (step x0 x1, step y0 y1)

step :: Int -> Int -> Int
step target x
  | x < target = x + 1
  | x > target = x - 1
  | otherwise  = x

task :: Int -> String -> Int
task n
  = parseInput inputParser
 .> map cmdSem
 .> flip applyAll (start n)
 .> tailVisited
 .> S.size

task1 :: String -> Int
task1 = task 2


example2 :: String
example2 = [__i|
  R 5
  U 8
  L 8
  D 3
  R 17
  D 10
  L 25
  U 20
|]


task2 :: String -> Int
task2 = task 10