{-# language QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Day10 where

import AoCPrelude
import Data.List
import Data.List.Extra

exampleSmall :: String
exampleSmall = [__i|
  noop
  addx 3
  addx -5
|]

example :: String
example = [__i|
  addx 15
  addx -11
  addx 6
  addx -3
  addx 5
  addx -1
  addx -8
  addx 13
  addx 4
  noop
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx -35
  addx 1
  addx 24
  addx -19
  addx 1
  addx 16
  addx -11
  noop
  noop
  addx 21
  addx -15
  noop
  noop
  addx -3
  addx 9
  addx 1
  addx -3
  addx 8
  addx 1
  addx 5
  noop
  noop
  noop
  noop
  noop
  addx -36
  noop
  addx 1
  addx 7
  noop
  noop
  noop
  addx 2
  addx 6
  noop
  noop
  noop
  noop
  noop
  addx 1
  noop
  noop
  addx 7
  addx 1
  noop
  addx -13
  addx 13
  addx 7
  noop
  addx 1
  addx -33
  noop
  noop
  noop
  addx 2
  noop
  noop
  noop
  addx 8
  noop
  addx -1
  addx 2
  addx 1
  noop
  addx 17
  addx -9
  addx 1
  addx 1
  addx -3
  addx 11
  noop
  noop
  addx 1
  noop
  addx 1
  noop
  noop
  addx -13
  addx -19
  addx 1
  addx 3
  addx 26
  addx -30
  addx 12
  addx -1
  addx 3
  addx 1
  noop
  noop
  noop
  addx -9
  addx 18
  addx 1
  addx 2
  noop
  noop
  addx 9
  noop
  noop
  noop
  addx -1
  addx 2
  addx -37
  addx 1
  addx 3
  noop
  addx 15
  addx -21
  addx 22
  addx -6
  addx 1
  noop
  addx 2
  addx 1
  noop
  addx -10
  noop
  noop
  addx 20
  addx 1
  addx 2
  addx 2
  addx -6
  addx -11
  noop
  noop
  noop
|]

data Instr
  = NOOP
  | Addx Int
  deriving (Show, Eq)

data State = MkS
  { x :: Int
  , instrs :: [(Int, Instr)]
  } deriving (Show, Eq)

inputParser :: Parser [Instr]
inputParser = everyLine $ choice
  [ NOOP <$ chunk "noop"
  , Addx <$ chunk "addx " <*> intSigned
  ]

withExecTime :: Instr -> (Int, Instr)
withExecTime = \case
  NOOP -> (1, NOOP)
  (Addx dx) -> (2, Addx dx)

-- Terminate on Nothing
step :: State -> Maybe State
step s@MkS{x, instrs} = case instrs of
  [] -> Nothing
  ((t,instr):instrs')
    -> Just (if t <= 1
              then case instr of
                      NOOP -> s{instrs = instrs'}
                      Addx dx -> s{x = x + dx, instrs = instrs'}
              else s{instrs = (t-1, instr):instrs'})

-- task1 :: String -> Int
task1 :: String -> Int
task1
  = parseInput inputParser
 .> map withExecTime
 .> MkS 1
 .> scanUntilNothing step
 .> map x
 .> zip [1..]
 .> filter (\(cycle, _) -> selector cycle)
 .> map (uncurry (*))
 .> sum
  where
    selector 0 = False
    selector n = (n - 20) `mod` 40 == 0

task2
  = parseInput inputParser
 .> map withExecTime
 .> MkS 1
 .> scanUntilNothing step
 .> init
 .> map x
 .> zip [1..]
 .> map (uncurry renderPixel)
 .> chunksOf 40
 .> unlines



renderPixel :: Int -> Int -> Char
renderPixel cycle x
  | within 1 crtIndex x = '#'
  | otherwise = '.'
  where
    crtIndex = (cycle - 1) `mod` 40