{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Day4 where

import AoCPrelude
import Control.Monad (void)
import Data.List (intersect)


example :: String
example = [__i|
  2-4,6-8
  2-3,4-5
  5-7,7-9
  2-8,3-7
  6-6,4-6
  2-6,4-8
|]

data AssignmentPair = MkAP (Int, Int) (Int, Int)
  deriving (Show, Eq)

inputParser :: Parser [AssignmentPair]
inputParser = everyLine assignmentPair

assignmentPair :: Parser AssignmentPair
assignmentPair
  = MkAP <$> assignment
         <*  char ','
         <*> assignment 

assignmentPairMonad :: Parser AssignmentPair
assignmentPairMonad = do 
  x <- assignment
  char ','
  y <- assignment
  pure (MkAP x y)

assignment :: Parser (Int, Int)
assignment = (,) <$> int <* char '-' <*> int

assignmentMonad :: Parser (Int, Int)
assignmentMonad = do
  x <- int
  char '-'
  y <- int
  pure (x,y)

fullyOverlaps :: AssignmentPair -> Bool
fullyOverlaps (MkAP (x0,x1) (y0,y1))
  = x0 <= y0 && y1 <= x1
 || y0 <= x0 && x1 <= y1

task1 :: String -> Int
task1
  = parseInput inputParser
  .> filter fullyOverlaps
  .> length

overlaps :: AssignmentPair -> Bool
overlaps (MkAP (x0,x1) (y0,y1))
  = x0 <= y0 && y0 <= x1
 || x0 <= y1 && y1 <= x1
 || y0 <= x0 && x0 <= y1
 || y0 <= x1 && x1 <= y1

task2 :: String -> Int
task2
  = parseInput inputParser
  .> filter overlaps
  .> length
