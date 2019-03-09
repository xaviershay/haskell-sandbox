module Main where

-- Following along with https://arxiv.org/pdf/1902.06776.pdf
-- Doesn't actually do anything

import qualified Data.Vector as V

type Register = Int
type ServerId = Int
type Value = Char

data ServerValue a =
    Unwritten
  | Written a
  | WrittenNil

data Decision a =
  Any
  | None
  | Tentative a
  | Decided a

type Config =
  [ (Register -> Bool, [[ServerId]])
  ]

data Client = Client
  { _clientState :: [(Register, ServerId)]
  }

data Server = Server
  { _serverState :: V.Vector (ServerValue Value)
  }

data Request =
    Read ServerId Register
  | Write ServerId Register

data Response = RegisterValue (ServerValue Value)

main = putStrLn "Hello"
