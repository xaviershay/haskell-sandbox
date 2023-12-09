module Aoc2023.Day4 where

import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace
import Data.List (foldl')
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Text.Parsec

type Game = (S.HashSet Int, S.HashSet Int)

day4 = testGroup "Day 4"
  [ testCase "Example" $ do
        x <- part1 "data/aoc2023/day4/example.txt"
        x @?= 13
  , testCase "Input" $ do
        x <- part1 "data/aoc2023/day4/input.txt"
        x @?= 25231
--  , testCase "Part 2 Example" $ do
--        x <- calculatePower "data/aoc2023/day2/example.txt"
--        x @?= 2286
--  , testCase "Part 2 Input" $ do
--        x <- calculatePower "data/aoc2023/day2/input.txt"
--        x @?= 67335
  ]

part1 filename =
  sum
  . map (scoreGame . parseUnsafe game)
  . lines <$> readFile filename
  
parseUnsafe :: Parsec SourceName () a -> SourceName -> a
parseUnsafe parser input =
  case parse parser input input of
    Right x -> x
    Left x -> error $ show x

scoreGame :: Game -> Int
scoreGame (winning, drawn) =
  let matches = S.size $ S.intersection winning drawn in

  if matches > 0 then
    2 ^ (matches - 1)
  else
    0

-- Parsers
game = do
  string "Card"
  spaces
  _ <- many1 digit
  string ":"
  spaces

  -- This is a bit weird coz `sepBy` can't work by itself to pickup the |
  -- separator (I don't think)
  winningNumbers <-
    S.fromList . map read <$> manyTill (many1 digit <* spaces) (string "|" <* spaces)
  drawnNumbers <-
    S.fromList . map read <$> many1 digit `sepBy` spaces

  return (winningNumbers, drawnNumbers)

