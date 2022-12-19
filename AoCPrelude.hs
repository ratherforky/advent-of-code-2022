{-# language QuasiQuotes #-}
module AoCPrelude (
  runDay,
  module Data.String.Interpolate,
  module Flow,
  -- Parsing
  Parser, parseInput, int, intSigned, everyLine,
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  module Text.Pretty.Simple,
  -- Misc
  applyN, applyAll, applyNScan,
  scanUntilNothing,
  within
  ) where

import Data.String.Interpolate
import Flow
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Data.Maybe
import Text.Pretty.Simple
import Data.Foldable

runDay :: Int -> (String -> b) -> IO b
runDay day task = task <$> readFile [i|Day#{day}-Input.txt|]

type Parser = Parsec Void String

parseInput :: Parser a -> String -> a
parseInput parser = parseMaybe parser .> fromJust

everyLine :: Parser a -> Parser [a]
everyLine parser = many (parser <* (void newline <|> eof))

int :: Parser Int
int = label "positive int"
    $ read <$> some numberChar

intSigned :: Parser Int
intSigned
  = label "signed int"
  $ choice
      [ char '-' *> (negate <$> int)
      , int
      ]

applyN :: Int -> (a -> a) -> a -> a
applyN n0 f = go n0
  where
    go 0 a = a
    go n a = go (n-1) (f a)

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl' (flip ($)) x fs

applyNScan :: Int -> (a -> a) -> a -> [a]
applyNScan n0 f = go n0
  where
    go 0 a = [a]
    go n a = a : go (n-1) (f a)

scanUntilNothing :: (a -> Maybe a) -> a -> [a]
scanUntilNothing f = go
  where
    go x = case f x of
      Nothing -> [x]
      Just x' -> x : go x'


within :: Int -> Int -> Int -> Bool
within n x y = abs (x - y) <= n
