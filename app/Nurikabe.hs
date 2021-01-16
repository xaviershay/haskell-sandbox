{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Data.IxSet hiding (groupBy)

import           Text.Parsec            hiding (many, optional)
import           Text.Parsec.Expr
import Data.List hiding (insert)
import Data.Function (on)
import Data.Ord

type Location = (Int, Int)

data CellData =
    Unknown
  | Wall
  | Open
  | OpenN Int
  deriving (Eq, Ord)

instance Show CellData where
  show Unknown = "_"
  show Wall = "*"
  show Open = "!"
  show (OpenN x) = show x


data Cell = Cell {
  location :: Location,
  contents :: CellData
} deriving (Eq, Ord)

instance Show Cell where
  show cell = "<" <> show cell.location <> " " <> show cell.contents <> ">"

type Shape = IxSet Cell
type Board = Shape

-- Needs solver context because can only expand into unknown (or open?)
--expand :: Shape -> Solver Shape

--step :: Solver Board

--step :: Board -> (AppliedRule, Board)
--runSolve :: Board -> ([AppliedRule], Board)

instance Indexable Cell where
  empty = ixSet
            [ ixFun (\c -> [c.location])
            , ixFun (\c -> [c.contents])
            ]

mkCell :: Location -> CellData -> Cell
mkCell loc contents = Cell {
  location = loc,
  contents = contents
}

derpCell = mkCell (0, 0) Unknown

testSet = updateIx (derpCell.location) derpCell empty
main = do
  putStrLn . show $ testSet @= Unknown
  case fmap toBoard $ parse boardParser "" "3_!\n24*" of
    Right board -> putStrLn (toAscii board)
    Left pe -> putStrLn . show $ pe

toAscii :: Board -> String
toAscii board =
    intercalate "\n"
  $ map (intercalate "" . map (show . contents) . sortBy (comparing (fst . location)))
  $ groupBy (on (==) (snd . location))
  $ sortBy (comparing (snd . location))
  $ toList board

toBoard :: [[CellData]] -> Board
toBoard rows =
  let indexed = zip [0..] $ map (\row -> zip [0..] row) rows in
  foldr insert empty
    $ concatMap (\(y, row) -> map (\(x, contents) -> mkCell (x, y) contents) row) indexed

toInt :: Char -> Int
toInt x = read [x]

readUnknown = Unknown <$ char '_'
readWall = Wall <$ char '*'
readOpen = Open <$ char '!'
readOpenN = OpenN . toInt <$> digit

readCell = readOpen <|> readUnknown <|> readWall <|> readOpenN

boardParser = sepBy (many1 readCell) endOfLine
