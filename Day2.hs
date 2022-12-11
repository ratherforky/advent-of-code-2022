{-# language QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Day2 where

import AoCPrelude
import Data.Bifunctor

example :: String
example = [__i|
  A Y
  B X
  C Z
|]

data ABC = A | B | C deriving (Show, Eq)
data XYZ = X | Y | Z deriving (Show, Eq)
data RPS = R | P | S deriving (Show, Eq)

class IsRPS a where
  toRPS :: a -> RPS

instance IsRPS ABC where
  toRPS = \case
    A -> R
    B -> P
    C -> S

instance IsRPS XYZ where
  toRPS = \case
    X -> R
    Y -> P
    Z -> S


inputParser :: String -> [(ABC,XYZ)]
inputParser
  = lines
  .> map (\case [abc,' ',xyz] -> (parseABC abc, parseXYZ xyz))

parseABC :: Char -> ABC
parseABC = \case
  'A' -> A
  'B' -> B
  'C' -> C

parseXYZ :: Char -> XYZ
parseXYZ = \case
  'X' -> X
  'Y' -> Y
  'Z' -> Z

getParsedInput :: IO [(ABC, XYZ)]
getParsedInput = inputParser <$> readFile "Day2-Input.txt"

run task = task <$> getParsedInput

task1 :: [(ABC, XYZ)] -> Int
task1 = map (bimap toRPS toRPS .> score) .> sum

score :: (RPS, RPS) -> Int
score (x, y) = outcome + shape
  where
    outcome 
      = if x == y
          then 3
          else case (x, y) of
            (R, P) -> 6
            (P, S) -> 6
            (S, R) -> 6
            _      -> 0
    
    shape = case y of
      R -> 1
      P -> 2
      S -> 3

task2 :: [(ABC, XYZ)] -> Int
task2 = map (task2Interpretation .> score) .> sum

task2Interpretation :: (ABC, XYZ) -> (RPS, RPS)
task2Interpretation (abc, result) = case result of
  X -> (rps, lose rps)
  Y -> (rps, rps)
  Z -> (rps, beat rps)
  where
    rps = toRPS abc
    beat = \case
      R -> P
      P -> S
      S -> R
    
    lose = \case
      R -> S
      P -> R
      S -> P
  
