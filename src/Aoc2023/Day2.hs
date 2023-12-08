module Aoc2023.Day2 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (foldl')
import qualified Data.HashMap.Strict as M

import Text.Parsec

parseUnsafe :: Parsec SourceName () a -> SourceName -> a
parseUnsafe parser input =
  case parse parser input input of
    Right x -> x
    Left x -> error $ show x

type Bag = M.HashMap String Int

day2 = testGroup "Day 2"
  [ testCase "Example" $ do
        x <- calculateValid "data/aoc2023/day2/example.txt"
        x @?= 8
  , testCase "Input" $ do
        x <- calculateValid "data/aoc2023/day2/input.txt"
        x @?= 2512
  , testCase "Part 2 Example" $ do
        x <- calculatePower "data/aoc2023/day2/example.txt"
        x @?= 2286
  , testCase "Part 2 Input" $ do
        x <- calculatePower "data/aoc2023/day2/input.txt"
        x @?= 67335
  ]
  where
    calculatePower filename =
      sum
      . map power
      . map (parseUnsafe game)
      . lines <$> readFile filename

    calculateValid filename =
      sum
      . map fst
      . filter (validForBag $
          M.fromList [("red", 12), ("green", 13), ("blue", 14)])
      . map (parseUnsafe game)
      . lines <$> readFile filename

    validForBag :: Bag -> (Int, [Bag]) -> Bool
    validForBag bag (_, draws) =
      all (all (>= 0))
      . map M.elems
      . map (\draw -> M.unionWith (-) bag draw)
      $ draws

    power :: (Int, [Bag]) -> Int
    power (_, draws) =
      let minBag = foldl'
                     (\m x -> M.unionWith max m x)
                     (M.fromList [("red", 0), ("green", 0), ("blue", 0)])
                     draws
      in

      foldl' (*) 1 . M.elems $ minBag

    -- Parsers
    colorResult = do
      n <- read <$> many1 digit
      string " "
      color <- string "red" <|> string "green" <|> string "blue"

      return (color, n)

    draw = M.fromList <$> colorResult `sepBy1` (string ", ")

    game = do
       string "Game "
       gameId <- read <$> many1 digit
       string ": "

       drawResult <- draw `sepBy1` string "; "

       return (gameId, drawResult)


