{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Text.Parsec            hiding ( optional)
import           Text.Parsec.Expr
import qualified Data.Vector            as V
import qualified Data.Map               as M
import Data.Maybe (fromJust)

type UserString = String

data Token = CommandToken Command | Comment String | Whitespace | Label String

data Command =
  PrintByte |
  ReadNum |
  Dup |
  PushVar String |
  StoreVar String |
  Call |
  Jump |
  Noop |
  Exit
  deriving (Show)

data Program = Program {
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
    Right x -> do
      putStrLn $ show x
      eval x


eval :: Program -> IO ()
eval p = do
  case commands p V.!? instructionPointer p of
    Nothing -> do
      putStrLn ""
      putStrLn . show $ p
      return ()
    Just c -> do
      putStrLn . show $ c
      p' <- evalCommand p { instructionPointer = instructionPointer p + 1 } c
      eval p'

evalCommand p Noop = return p
evalCommand p ReadNum = return $ p { stack = 42:stack p }
evalCommand p PrintByte = do
  let (byte:stack') = stack p
  putStr . show $ byte
  return p { stack = stack' }

evalCommand p Exit = return $ p { instructionPointer = V.length (commands p) }
--evalCommand p Dup = return $ p { stack = top:s }
--  where
--    s = stack p
--    top = head s

evalCommand p (StoreVar x) = return $ p { variables = M.insert x top (variables p), stack = stack' }
  where
    (top:stack') = stack p

evalCommand p (PushVar x) = return $ p { stack = value:stack p}
  where
    value = case M.lookup x (variables p) of
      Nothing -> error ("Failed to lookup: " ++ x)
      Just x  -> x

evalCommand p Call = return $ p { instructionPointer = location, stack = (instructionPointer p):stack'}
  where
    (location:stack') = stack p

evalCommand p Jump = return $ p { instructionPointer = location, stack = stack' }
  where
    (location:stack') = stack p

parse17 :: UserString -> ParseResult
parse17 input = runParser xxx () input input

--grammar = many (comment <|> try label17 <|> command <|> variable <|> whitespace)


xxx = do
  stream <- many (comment <|> try label17 <|> command <|> variable <|> whitespace)

  return Program {
    variables = filterLabels 0 M.empty stream,
    commands = V.fromList (identifyStores . filterCommands $ stream),
    stack = [],
    instructionPointer = 0
  }

identifyStores :: [Command] -> [Command]
identifyStores (PushVar x:StoreVar y:cs)= StoreVar x:Noop:identifyStores cs
identifyStores (StoreVar y:cs) = undefined -- parse error
identifyStores (c:cs) = c:identifyStores cs
identifyStores [] = []

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
  <|> makeCommand "dup" Dup
  <|> makeCommand "jump" Jump
  <|> makeCommand "call" Call
  <|> makeCommand "store" (StoreVar "")

makeCommand x t = do
  try (string x)

  return $ CommandToken t
 
variable = do
  x <- many1 alphaNum
  return $ CommandToken (PushVar x)
