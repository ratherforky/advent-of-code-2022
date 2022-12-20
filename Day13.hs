{-# language QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Day13 where

import AoCPrelude
import Data.List.Extra (chunksOf)
import Data.List

type Packet = [PacketData]

data PacketData
  = List [PacketData]
  | Lit Int
  deriving (Show, Eq) 

example :: String
example = [__i|
  [1,1,3,1,1]
  [1,1,5,1,1]

  [[1],[2,3,4]]
  [[1],4]

  [9]
  [[8,7,6]]

  [[4,4],4,4]
  [[4,4],4,4,4]

  [7,7,7,7]
  [7,7,7]

  []
  [3]

  [[[]]]
  [[]]

  [1,[2,[3,[4,[5,6,7]]]],8,9]
  [1,[2,[3,[4,[5,6,0]]]],8,9]
|]


exampleSmall :: String
exampleSmall = [__i|
  [[2,[],3]]
  [[]]
|]



packets :: Parser [Packet]
packets = manyIgnoreWhitespace packet

packet :: Parser Packet
packet = char '[' *> (delim packetDataP "," (pure ()) <|> pure []) <* char ']'

packetDataP :: Parser PacketData
packetDataP = choice
  [ List <$> choice
               [ [] <$ tok "[]"
               , tok "[" *> delim packetDataP "," (tok "]")
               ]
  , Lit <$> int
  ]

task1
  =  parseInput packets
  .> chunksOf 2
  .> zip [1..]
  .> filter (\(_,[pLeft, pRight]) -> comparePackets pLeft pRight == LT)
  .> map fst
  .> sum

task2
  =  parseInput packets
  .> (decoderPackets ++)
  .> sortBy comparePackets
  .> zip [1..]
  .> filter (\(_,p) -> p `elem` decoderPackets)
  .> map fst
  .> product

decoderPackets :: [Packet]
decoderPackets = map divPacket [2, 6]

divPacket :: Int -> Packet
divPacket x = [List [Lit x]]

comparePackets :: Packet -> Packet -> Ordering
comparePackets pLeft pRight = comparePacketData (List pLeft) (List pRight)

comparePacketData :: PacketData -> PacketData -> Ordering
comparePacketData pLeft pRight = case (pLeft, pRight) of
  (Lit x,   Lit y) -> if | x == y -> EQ
                         | x <  y -> LT
                         | otherwise -> GT 
  (Lit x,   List ys) -> comparePacketData (List [Lit x]) (List ys)
  (List xs, Lit y)   -> comparePacketData (List xs) (List [Lit y])
  (List xs, List ys) -> zipCompare xs ys
  where
    zipCompare []     []     = EQ
    zipCompare []     (_:_)  = LT
    zipCompare (_:_)  []     = GT
    zipCompare (x:xs) (y:ys) = case comparePacketData x y of
      EQ -> zipCompare xs ys
      c -> c
