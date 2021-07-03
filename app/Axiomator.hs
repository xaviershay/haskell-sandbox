{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.Hashable
import GHC.Generics
import qualified Data.Tuple
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Monoid ((<>))
import Control.Monad (msum, forM_)
import Text.Parsec( runParser, char, (<|>), ParseError, many, many1, digit, eof, choice)

import Control.Monad.Freer (Eff, Members, run, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)
import Control.Monad.Freer.State (State(..), get, gets, put, runState)
import Control.Monad.Freer.Writer (Writer(..), tell, runWriter)

--data Axiom =
--  CommuteSum |
--  AssociateSumLR |
--  AssociateSumRL |
--  CombineConst

data Axiom = Axiom {
  description :: String,
  example :: (Term, Term),
  implementation :: Term -> Either Term Term
}

instance Show Axiom where
  show (Axiom { description = d }) = d

axiomCommuteSum = Axiom {
  description = "Commutative law for addition",
  example = (Sum (Const 1) (Const 2), Sum (Const 2) (Const 1)),
  implementation = f
}
  where
    f (Sum a b) = Right (Sum b a)
    f t = Left t

axiomAssociateSum = Axiom {
  description = "Associative law for addition",
  example = (
    Sum (Const 1) (Sum (Const 2) (Const 3)),
    Sum (Sum (Const 1) (Const 2)) (Const 3)
  ),
  implementation = f
}
  where
    f (Sum (Sum a b) c) = Right (Sum a (Sum b c))
    f (Sum a (Sum b c)) = Right (Sum (Sum a b) c)
    f t = Left t

axiomSumConst = Axiom {
  description = "Sum constants",
  example = (
    Sum (Const 1) (Const 2),
    (Const 3)
  ),
  implementation = f
}
  where
    f (Sum (Const a) (Const b)) = Right (Const $ a + b)
    f t = Left t

allAxioms =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomSumConst
  ]

data Term =
  Const Integer |
  Sum Term Term
  deriving (Show, Eq)

data Matcher =
  RootMatcher
  | LeftMatcher Term

modify :: Matcher -> (Term -> Either Term Term) -> Term -> Either Term Term
modify (LeftMatcher x) f t@(Sum a b) | x == a = f t
modify RootMatcher f t = f t
modify m f (Sum a b) =
  let
    rhs = modify m f a
    lhs = modify m f b
  in
    Sum <$> rhs <*> lhs
modify m f (Const a) = Right (Const a)
modify m f t = Left t

toAscii (Const a) = show a
toAscii (Sum a b) = "(" <> toAscii a <> " + " <> toAscii b <> ")"

--main = putStrLn . toAscii
--  -- . modify (LeftMatcher (Const 3)) foldConst
--  . modify (LeftMatcher (Const 1)) foldConst
--  . commuteSum
--  . associateSumRL
--  $ Sum (Const 1) (Sum (Const 2) (Const 3))

--apply (Term -> Term) -> Workspace Term


data Env = Env Term deriving (Show)

type Log = [(Term, Axiom)]
type AppEff effs = Members '[ Writer Log, State Env ] effs

apply :: AppEff effs => Matcher -> Axiom -> Eff effs ()
apply m axiom = do
  (Env t) <- get

  -- TODO: Handle error
  let (Right t') = modify m (implementation axiom) t
  tell [(t', axiom)]

  put (Env t')

working = do
  apply RootMatcher axiomCommuteSum
  apply RootMatcher axiomAssociateSum
  apply RootMatcher axiomAssociateSum
  apply (LeftMatcher (Const 2)) axiomSumConst
  apply RootMatcher axiomSumConst

main = do
  forM_ (zip allAxioms [1..]) $ \(axiom, i) -> do
    putStr (show i)
    putStr ". "
    putStr (description axiom)
    putStr ": "
    let (lhs, rhs) = example axiom
    putStr $ toAscii lhs
    putStr " = "
    putStrLn $ toAscii rhs

  putStrLn ""

  let t = Sum (Const 1) (Sum (Const 2) (Const 3))
  putStrLn . toAscii $ t
  let (_, log) = runApp (Env t) working
  forM_ log $ \(t, axiom) -> do
    putStr (toAscii t)
    putStrLn $ "\t; " <> description axiom

runApp :: Env -> Eff '[ Writer Log, State Env] a -> (Term, Log)
runApp env m = do
  let ((_, log), (Env t)) = run . runState env . runWriter $ m

  (t, log)
