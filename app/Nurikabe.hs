{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Data.IxSet hiding (groupBy)

import           Text.Parsec            hiding (many, optional)
import           Text.Parsec.Expr
import Data.List hiding (insert, delete, null)
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

data CellAttribute = KnownSize
  deriving (Eq, Ord)

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
            , ixFun (\c -> case c.contents of
                             (OpenN _) -> [KnownSize]
                             _ -> [])
            ]

mkCell :: Location -> CellData -> Cell
mkCell loc contents = Cell {
  location = loc,
  contents = contents
}

neighbours :: Cell -> Shape -> Shape
neighbours = undefined

allNeighbours :: Cell -> Shape -> Shape
allNeighbours c shape = allNeighbours' (insert c empty) shape

allNeighbours' :: Shape -> Shape -> Shape
allNeighbours' acc remaining =
  case (toList acc, toList remaining) of
    ([], _) -> error "shouldn't happen"
    (x, []) -> acc
    ((x:xs), _) -> let
                       ns = neighbours x remaining
                       newRem = foldr delete remaining (toList ns)
                     in
                       if Data.IxSet.null ns then
                         acc
                       else
                         allNeighbours' (acc ||| ns) newRem

--getKnownShapes :: Shape -> [Shape]
--getKnownShapes board =
--  let
--    remaining = board @= KnownSize
--  in
--
--  case toList remaining of
--    [] -> []
--    (x:xs) ->
--      let ns = neighbours x remaining in

-- Get list of all OpenN cells
-- Group into shapes
-- collect until remaining is empty
--   - place first element into queue
--   - pop head of queue
--   - remove from remaining
--   - add to current shape
--   - get neighbours (from remaining)
--       add to back of queue
-- [Shape]
-- Filter down to complete ones
-- For each, create walls from all unknown neighbours

main = do
  case fmap toBoard $ parse boardParser "" "1_" of
    Right board -> do
      putStrLn . show $ board @= KnownSize
      putStrLn (toAscii board)
    Left pe -> putStrLn . show $ pe

toAscii :: Board -> String
toAscii =
    intercalate "\n"
  . map (intercalate "" . map (show . contents) . sortBy (comparing (fst . location)))
  . groupBy (on (==) (snd . location))
  . sortBy (comparing (snd . location))
  . toList

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
