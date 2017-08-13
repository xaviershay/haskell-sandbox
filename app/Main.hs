{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Text.Parsec            hiding ( optional)
import           Text.Parsec.Expr
import qualified Data.Vector            as V
import qualified Data.Map               as M

type UserString = String

data Token = CommandToken Command | Comment String | Whitespace | Label String | Variable String

data Command =
  PrintByte |
  ReadNum |
  Exit
  deriving (Show)

data Program = Program {
  labelMap :: M.Map String Int,
  commands :: V.Vector Command,
  stack :: [Int],
  variables :: M.Map String Int,
  instructionPointer :: Int
} deriving (Show)

type ParseResult = Either ParseError Program

main :: IO ()
main = do
  contents <- readFile "test.17"

  case parse17 contents of
    Left x -> putStrLn $ show x
    Right x -> putStrLn . show $ x


evalCommand p (Command Exit) = p { instructionPointer = V.length (commands p) }

parse17 :: UserString -> ParseResult
parse17 input = runParser xxx () input input

--grammar = many (comment <|> try label17 <|> command <|> variable <|> whitespace)


xxx = do
  stream <- many (comment <|> try label17 <|> command <|> variable <|> whitespace)

  return Program {
    labelMap = filterLabels 0 M.empty stream,
    commands = V.fromList (filterCommands stream)
  }

filterLabels :: Int -> M.Map String Int -> [Token] -> M.Map String Int
filterLabels i m ((CommandToken c):xs) = filterLabels (i+1) m xs
filterLabels i m ((Label c):xs) = filterLabels i (M.insert c i m) xs
filterLabels i m (x:xs) = filterLabels i m xs
filterLabels i m [] = m

filterCommands :: [Token] -> [Command]
filterCommands ((CommandToken c):xs) = c:(filterCommands xs)
filterCommands (x:xs) = filterCommands xs
filterCommands [] = []

whitespace = do
  many1 space
  return Whitespace

comment = do
  string "/*"
  x <- manyTill anyChar (try (string "*/"))
  return $ Comment x

label17 = do
  x <- many1 alphaNum
  string ":"

  return $ Label x

command = do
      makeCommand "print_byte" PrintByte
  <|> makeCommand "read_num" ReadNum
  <|> makeCommand "exit" Exit

makeCommand x t = do
  try (string x)

  return $ CommandToken t
 
variable = do
  x <- many1 alphaNum
  return $ Variable x
