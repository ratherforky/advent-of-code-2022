{-# language QuasiQuotes #-}
module AoCPrelude (
  runDay,
  module Data.String.Interpolate,
  module Flow,
  -- Parsing
  Parser, parseInput, int, everyLine,
  module Text.Megaparsec,
  module Text.Megaparsec.Char
  ) where

import Data.String.Interpolate
import Flow
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Data.Maybe

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