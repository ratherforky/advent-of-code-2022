{-# language QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Day9 where

import AoCPrelude
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative

data CMD = MkCMD Dir Int

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
  { headPos :: Coord
  , tailPos :: Coord
  -- , ropePos :: [Coord]
  , tailVisited :: Set Coord
  } deriving (Show, Eq)

type Coord = (Int, Int)

start :: StateHT
start = MkS (0,0) (0,0) (S.singleton (0,0))

-- l,r,u,d :: StateHT -> StateHT
-- l = move (-1) 0
-- r = move 1 0
-- u = move 0 1
-- d = move 0 (-1)


move :: Int -> Int -> StateHT -> StateHT
move dx dy (MkS (x0,y0) (x1,y1) visited) = MkS{..}
  where
    headPos = (x0+dx,y0+dy)
    tailPos
      | dx /= 0 = if x0 == x1 + dx
                  then (x0,y0)
                  else (x1,y1)
      | dy /= 0 = if y0 == y1 + dy
                  then (x0,y0)
                  else (x1,y1)
    tailVisited = S.insert tailPos visited

dirSem :: CMD -> StateHT -> StateHT
dirSem (MkCMD d n) = applyN n
  (case d of
    L -> move (-1) 0
    R -> move 1 0
    U -> move 0 1
    D -> move 0 (-1)
  )

task1 :: String -> Int
task1 = parseInput inputParser
     .> map dirSem
     .> flip applyAll start
     .> tailVisited
     .> S.size
