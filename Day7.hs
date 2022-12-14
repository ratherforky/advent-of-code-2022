{-# language QuasiQuotes #-}
module Day7 where

import AoCPrelude
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.List (sort)

-- Input


example :: String
example = [__i|
  $ cd /
  $ ls
  dir a
  14848514 b.txt
  8504156 c.dat
  dir d
  $ cd a
  $ ls
  dir e
  29116 f
  2557 g
  62596 h.lst
  $ cd e
  $ ls
  584 i
  $ cd ..
  $ cd ..
  $ cd d
  $ ls
  4060174 j
  8033020 d.log
  5626152 d.ext
  7214296 k

|]

example2 :: String
example2 = [__i|
  $ ls
  4060174 j
  8033020 d.log
  5626152 d.ext
  7214296 k
|]

example3 :: String
example3 = [__i|
  $ ls
  dir a
  14848514 b.txt
  8504156 c.dat
  dir d
|]



data TerminalOutput
  = CD Path
  | LS [FlatFile]
  deriving (Show, Eq)

data FlatFile
  = DirFF String
  | FileFF Int String
  deriving (Show, Eq)

data Path
  = DirName String
  | DotDot
  | Root
  deriving (Show, Eq)

inputParser :: Parser [TerminalOutput]
inputParser = many (terminalOutput <* (void space <|> eof))

terminalOutput :: Parser TerminalOutput
terminalOutput
  = chunk "$ "
 *> choice
      [ CD <$ chunk "cd" <* spaceChar <*> path
      , LS <$ chunk "ls" <* newline <*> flatFiles
      ]

path :: Parser Path
path = choice
  [ Root <$ char '/'
  , DotDot <$ chunk ".."
  , DirName <$> takeWhile1P (Just "Directory name") (/= '\n')
  ]

flatFiles :: Parser [FlatFile]
flatFiles
  = many (flatFile <* (void newline <|> eof))

flatFile :: Parser FlatFile
flatFile = choice
  [ DirFF <$  chunk "dir"
          <*  hspace1
          <*> takeWhileP (Just "Directory name")
                         (/= '\n')
  , FileFF <$> int 
           <*  hspace1
           <*> takeWhileP (Just "File name")
                          (/= '\n')
  ]

-- Output structure

type FileTree = Map ID File
type ID = String
data File = Dir FileTree
          | File Int
          deriving (Show, Eq)

task1 :: String -> Int
task1 = parseInput inputParser
     .> toFileTree
     .> directorySizes
     .> filter (<= 100000)
     .> sum

task2 :: String -> Int
task2 = parseInput inputParser
     .> toFileTree
     .> directorySizes
     .> (\dirSizes@(rootSize:_) -> filter (rootSize - 40000000 <=) dirSizes)
     .> sort
     .> head

directorySizes :: FileTree -> [Int]
directorySizes
  = foldrFileTreeWithID
      (\_ (fullSubDirSize, subdirSizes) (accSize, accSizes) -> (accSize + fullSubDirSize, fullSubDirSize : subdirSizes ++ accSizes))
      (\_ fileSize (accSize, accSizes) -> (fileSize + accSize, accSizes))
      (0, [])
 .> snd

foldrFileTreeWithID :: (ID -> acc -> acc -> acc)
                    -> (ID -> Int -> acc -> acc)
                    -> acc
                    -> FileTree
                    -> acc
foldrFileTreeWithID dirFunc fileFunc def tree0
  = dirFunc "/" (foldrFileTreeWithID' tree0) def
  where
    foldrFileTreeWithID' = M.foldrWithKey f def
      where
        f ident file acc = case file of
          Dir tree' -> dirFunc ident (foldrFileTreeWithID' tree') acc
          File size -> fileFunc ident size acc

toFileTree :: [TerminalOutput] -> FileTree
toFileTree termOuts = go [] termOuts M.empty
  where
    go curDir [] tree = tree
    go curDir (cmdOut:outs) tree = case cmdOut of
      CD Root -> go [] outs tree
      CD DotDot -> go (init curDir) outs tree
      CD (DirName x) -> go (curDir ++ [x]) outs tree
      LS ffs -> go curDir outs (updateDir curDir ffs tree)

updateDir :: [String] -> [FlatFile] -> FileTree -> FileTree
updateDir [] ffs tree = foldl' (flip insertAtRoot) tree ffs
updateDir (dir:dirs) ffs tree = M.alter f dir tree
  where
    f :: Maybe File -> Maybe File
    f Nothing = Just $ Dir $ updateDir dirs ffs M.empty
    f (Just (Dir tree')) = Just $ Dir $ updateDir dirs ffs tree'
    f (Just (File _)) = error [i|Found file instead of dir at #{dir:dirs}|]

insertAtRoot :: FlatFile -> FileTree -> FileTree
insertAtRoot (DirFF ident) = insertIfNotPresent ident (Dir M.empty)
insertAtRoot (FileFF size ident) = insertIfNotPresent ident (File size)

insertIfNotPresent :: Ord k => k -> a -> Map k a -> Map k a
insertIfNotPresent = M.insertWith (\new old -> old)
