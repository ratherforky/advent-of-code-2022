{-# language QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Day11 where

import AoCPrelude
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Foldable
import Data.List (sortOn)
import Data.Ord

example :: String
example = [__i|
  Monkey 0:
    Starting items: 79, 98
    Operation: new = old * 19
    Test: divisible by 23
      If true: throw to monkey 2
      If false: throw to monkey 3

  Monkey 1:
    Starting items: 54, 65, 75, 74
    Operation: new = old + 6
    Test: divisible by 19
      If true: throw to monkey 2
      If false: throw to monkey 0

  Monkey 2:
    Starting items: 79, 60, 97
    Operation: new = old * old
    Test: divisible by 13
      If true: throw to monkey 1
      If false: throw to monkey 3

  Monkey 3:
    Starting items: 74
    Operation: new = old + 3
    Test: divisible by 17
      If true: throw to monkey 0
      If false: throw to monkey 1
|]

data Monkey = MkM
  { startingItems :: [Int]
  , op :: Op
  , test :: Test
  , inspections :: Int
  } deriving (Show, Eq)

data Test = MkTest
  { denom :: Int
  , tMonkey :: Int
  , fMonkey :: Int 
  } deriving (Show, Eq)

data Op = MkOp BinOp Expr
  deriving (Show, Eq)

data Expr = Old | Lit Int
  deriving (Show, Eq)
 
data BinOp = Add | Mul
  deriving (Show, Eq)

monkeys :: Parser (IntMap Monkey)
monkeys = IM.fromList <$> some (monkeyI <* space)

monkeyI :: Parser (Int, Monkey)
monkeyI = (,) <$  tok "Monkey" <*> int <* tok ":"
              <*> monkey

monkey :: Parser Monkey
monkey
  = MkM <$> startingItemsP
        <*> opP
        <*> testP
        <*> pure 0

startingItemsP :: Parser [Int]
startingItemsP
  =  tok "Starting items:"
  *> many (int <* (void newline <|> tok ","))

opP :: Parser Op
opP = MkOp
   <$  tok "Operation: new = old "
   <*> binOpP
   <*> exprP

exprP :: Parser Expr
exprP = choice
  [ Old <$ tok "old"
  , Lit <$> int
  ]

binOpP :: Parser BinOp
binOpP = choice
  [ Add <$ tok "+"
  , Mul <$ tok "*"
  ]

testP :: Parser Test
testP = MkTest
  <$ tok "Test: divisible by" <*> int
  <* tok "If true: throw to monkey" <*> int
  <* tok "If false: throw to monkey" <*> int

monkeyingAround :: Int -> Int -> IntMap Monkey -> IntMap Monkey
monkeyingAround modNum worryDenom = go 0  
  where
    go i monkMap = case monkMap IM.!? i of
      Nothing   -> monkMap
      Just monk -> let (monk', throws) = turn modNum worryDenom monk
                       monkMap' = monkMap
                                  |> IM.insert i monk'
                                  |> insertThrows throws
                   in go (i+1) monkMap'
 
clearItems :: Monkey -> Monkey
clearItems monk = monk{startingItems = []}

insertThrows :: [(Int,Int)] -> IntMap Monkey -> IntMap Monkey
insertThrows = map insertThrow .> applyAll

insertThrow :: (Int, Int) -> IntMap Monkey -> IntMap Monkey
insertThrow (i, val) = IM.adjust (\MkM{..} -> MkM{startingItems = val : startingItems, op, test, inspections}) i

turn :: Int -> Int -> Monkey -> (Monkey, [(Int, Int)])
turn modNum worryDenom MkM{startingItems, op, test, inspections}
  = ( MkM [] op test (inspections + fromIntegral (length startingItems))
    , map (opSem modNum op .> testSem worryDenom test) startingItems
    )

opSem :: Int -> Op -> (Int -> Int)
opSem modNum (MkOp binOp expr) old = binOpSem old exprSem `mod` modNum
  where
    binOpSem = case binOp of
      Add -> (+)
      Mul -> (*)

    exprSem = case expr of
      Old -> old
      Lit y -> y

testSem :: Int -> Test -> Int -> (Int,Int)
testSem worryDenom MkTest{..} val
  = if val' `mod` denom == 0
    then (tMonkey, val')
    else (fMonkey, val')
  where
    val' = val `div` worryDenom

calcModNum :: IntMap Monkey -> Int
calcModNum = IM.elems .> map (test .> denom) .> product

task :: Int -> Int -> String -> Int
task rounds worryDenom
  = parseInput monkeys
 .> (\monkMap -> applyN rounds (monkeyingAround (calcModNum monkMap) worryDenom) monkMap)
 .> IM.elems
 .> map inspections
 .> sortOn Down
 .> take 2
 .> product

task1, task2 :: String -> Int
task1 = task 20 3
task2 = task 10000 1
