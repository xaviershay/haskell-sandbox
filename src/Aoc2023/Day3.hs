module Aoc2023.Day3 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List (groupBy, foldl', sortBy)
import Data.Ord (comparing)
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S


type Cell = (Int, Int)

day3 = testGroup "Day 3"
  [ testCase "Example" $ do
      x <- part1 "data/aoc2023/day3/example.txt"
      x @?= 4361
  , testCase "Input" $ do
      x <- part1 "data/aoc2023/day3/input.txt"
      x @?= 537832
  , testCase "Example, part 2" $ do
      x <- part2 "data/aoc2023/day3/example.txt"
      x @?= 467835
  , testCase "Input, part 2" $ do
      x <- part2 "data/aoc2023/day3/input.txt"
      x @?= 81939900
  ]

locateCells f grid = V.toList $
  V.imap
    (\y row -> V.toList . V.map (\x -> (x, y)) $ V.findIndices f row)
    grid

part1 filename = do
  raw <- lines <$> readFile filename

  let grid = V.fromList (map V.fromList raw)

  let symbolLocations = concat $ locateCells isPart grid
  let numberLocations = groupAdjacent . concat $ locateCells isDigit grid

  -- Build a set of cell locations where if a digit exists in one, then the
  -- number that digit is apart of is a part.
  let possiblePartLocations =
        S.fromList
        . concatMap gridNeighbours
        $ symbolLocations

  let partLocations = filter
        (any (\x -> S.member x possiblePartLocations))
        numberLocations

  let result = sum . map (toNumber grid) $ partLocations

  return result

part2 filename = do
  raw <- lines <$> readFile filename

  let grid = V.fromList (map V.fromList raw)
  let symbolLocations = concat $ locateCells (== '*') grid
  let numberLocations = groupAdjacent . concat $ locateCells isDigit grid

  let possiblePartLocations =
        M.fromList
        . concatMap (\x -> zip (gridNeighbours x) (repeat x))
        $ symbolLocations

  let gears =
        map (map (toNumber grid . snd))
        . filter ((==) 2 . length)
        . groupBy (\x y -> fst x == fst y)
        . sortBy (comparing fst)
        . mapMaybe firstRef
        . map (\cs -> (cs, mapMaybe (\c -> M.lookup c possiblePartLocations) cs))
        $ numberLocations

  let result = sum . map (foldl' (*) 1) $ gears

  return result

firstRef (n, (x:_)) = Just (x, n)
firstRef _ = Nothing

toNumber :: V.Vector (V.Vector Char) -> [Cell] -> Int
toNumber grid cells =
  let digits = map (\(x, y) -> read . (:[]) $ (grid ! y) ! x) cells in
  sum . map (\(d, p) -> d * (10 ^ p)) $ zip (reverse digits) [0..]

-- This function is gross but it works
groupAdjacent :: [Cell] -> [[Cell]]
groupAdjacent xs = reverse $ groupAdjacent' [] [] xs
  where
    groupAdjacent' :: [[Cell]] -> [Cell] -> [Cell] -> [[Cell]]
    groupAdjacent' grouped candidate [] = (reverse candidate):grouped
    groupAdjacent' grouped [] (x:xs) = groupAdjacent' grouped [x] xs
    groupAdjacent' grouped candidate@((x1, y1):ys) remainder@((x2, y2):xs)
      | x2 == x1 + 1 = groupAdjacent' grouped ((x2, y2):candidate) xs
      | otherwise    = groupAdjacent' (reverse candidate:grouped) [(x2, y2)] xs

isPart :: Char -> Bool
isPart '.' = False
isPart x = (not . isDigit) x

gridNeighbours (x, y) =
  [ (x, y)
  , (x-1, y)
  , (x+1, y)
  , (x, y-1)
  , (x, y+1)
  , (x-1, y-1)
  , (x-1, y+1)
  , (x+1, y-1)
  , (x+1, y+1)
  ]
