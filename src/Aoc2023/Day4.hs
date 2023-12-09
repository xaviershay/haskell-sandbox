module Aoc2023.Day4 where

import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace
import Data.List (foldl', intercalate)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Text.Parsec

type Game = (Int, S.HashSet Int, S.HashSet Int)

day4 = testGroup "Day 4"
  [ testCase "Example" $ do
        x <- part1 "data/aoc2023/day4/example.txt"
        x @?= 13
  , testCase "Input" $ do
        x <- part1 "data/aoc2023/day4/input.txt"
        x @?= 25231
  , testCase "Part 2 Example" $ do
        x <- part2 "data/aoc2023/day4/example.txt"
        x @?= 30
  , testCase "Part 2 Input" $ do
        x <- part2 "data/aoc2023/day4/input.txt"
        x @?= 9721255
  ]

part1 filename =
  sum
  . map (scoreGame . parseUnsafe game)
  . lines <$> readFile filename

part2 filename = do
  cards <- map (parseUnsafe game) . lines <$> readFile filename

  let indexed = map (\g@(id, winning, drawn) -> (id, matches g)) $ cards
  let ids = map fst indexed

  let score = process (M.fromList $ zip ids (repeat 1)) indexed

  return score
  
process :: M.HashMap Int Int -> [(Int, Int)] -> Int
process copies [] = sum $ M.elems copies
process copies ((id, score):xs) =
  let
    n = fromJust $ M.lookup id copies
    additionalCopies = M.fromList $
        map (\newId -> (newId, n)) [id+1..id+score]

    totalCopies = M.unionWith (+) copies additionalCopies
  in

  process totalCopies xs

parseUnsafe :: Parsec SourceName () a -> SourceName -> a
parseUnsafe parser input =
  case parse parser input input of
    Right x -> x
    Left x -> error $ show x

matches :: Game -> Int
matches (_, winning, drawn) = S.size $ S.intersection winning drawn

scoreGame :: Game -> Int
scoreGame game =
  let x = matches game in

  if x > 0 then
    2 ^ (x - 1)
  else
    0

-- Parsers
game = do
  string "Card"
  spaces
  gameId <- read <$> many1 digit
  string ":"
  spaces

  -- This is a bit weird coz `sepBy` can't work by itself to pickup the |
  -- separator (I don't think)
  winningNumbers <-
    S.fromList . map read <$> manyTill (many1 digit <* spaces) (string "|" <* spaces)
  drawnNumbers <-
    S.fromList . map read <$> many1 digit `sepBy` spaces

  return (gameId, winningNumbers, drawnNumbers)

