{-# language QuasiQuotes #-}
module Day5 where

import AoCPrelude
import Data.Vector ( Vector )
import Data.Vector qualified as V
import Data.List (transpose)
import qualified Data.Vector as V
import Data.Maybe
import Data.Foldable

data Puzzle = MkPuzzle (Vector [Char]) [Instruction]
  deriving (Show, Eq)

-- Number to move
-- From Index
-- To Index
data Instruction = MkInstrs Int Int Int
  deriving (Show, Eq)

example :: String
example = [__i|
      [D]    
  [N] [C]    
  [Z] [M] [P]
  1   2   3 

  move 1 from 2 to 1
  move 3 from 1 to 3
  move 2 from 2 to 1
  move 1 from 1 to 2
|]

task1 :: String -> [Char]
task1
  = parseInput puzzle
  .> runPuzzle move
  .> fmap listToMaybe
  .> V.toList
  .> catMaybes

runPuzzle :: (Int -> [Char] -> [Char] -> ([Char], [Char])) -> Puzzle -> Vector [Char]
runPuzzle moveFunc (MkPuzzle startState instrs)
  = foldl' performInstr startState instrs
  where
    performInstr :: Vector [Char] -> Instruction -> Vector [Char]
    performInstr xss (MkInstrs n from to) = xss V.// [(from', fromNew), (to', toNew)]
      where
        from' = from - 1
        to' = to - 1

        fromInit = xss V.! from'
        toInit = xss V.! to'

        (fromNew, toNew) = moveFunc n fromInit toInit

move :: Int -> [a] -> [a] -> ([a], [a])
move n xs ys | n <= 0 = (xs,ys)
move n [] ys = ([], ys)
move n (x:xs) ys = move (n - 1) xs (x:ys) 

-- Task 2

task2 :: String -> [Char]
task2
  = parseInput puzzle
  .> runPuzzle move'
  .> fmap listToMaybe
  .> V.toList
  .> catMaybes

move' :: Int -> [a] -> [a] -> ([a], [a])
move' n xs ys = (xs', xsN ++ ys)
  where
    (xsN, xs') = splitAt n xs 


-- Parsing

puzzle :: Parser Puzzle
puzzle = MkPuzzle <$> stacks <*> instructions

crate :: Parser Char
crate = char '[' *> upperChar <* char ']'

crateMaybe :: Parser (Maybe Char)
crateMaybe = Just <$> crate
         <|> Nothing <$ chunk "   "

stacks :: Parser (Vector [Char])
stacks
  =  makeStacks <$> some (crateMaybe `sepBy` char ' ' <* newline) 
  <* takeWhileP Nothing (/= '\n')
  <* newline
  <* newline

makeStacks :: [[Maybe Char]] -> Vector [Char]
makeStacks
  =  transpose
  .> map catMaybes
  .> V.fromList

instruction :: Parser Instruction
instruction
  = MkInstrs <$  chunk "move "
             <*> int <* chunk " from "
             <*> int <* chunk " to "
             <*> int

instructions :: Parser [Instruction]
instructions = everyLine instruction