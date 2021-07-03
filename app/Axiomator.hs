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

data Term =
  Const Integer |
  Sum Term Term
  deriving (Show, Eq)

commuteSum (Sum a b) = (Sum b a)
associateSumLR (Sum (Sum a b) c) = (Sum a (Sum b c))
associateSumRL (Sum a (Sum b c)) = (Sum (Sum a b) c)
foldConst (Sum (Const a) (Const b)) = (Const $ a + b)
foldConst x = error $ "trying to fold " <> show x

data Matcher = LeftMatcher Term

modify :: Matcher -> (Term -> Term) -> Term -> Term
modify (LeftMatcher x) f t@(Sum a b) | x == a = f t
modify m f (Sum a b) = Sum (modify m f a) (modify m f b)
modify m f (Const a) = (Const a)
modify m f t = error $ "trying to modify " <> show t

toAscii (Const a) = show a
toAscii (Sum a b) = "(" <> toAscii a <> " + " <> toAscii b <> ")"

--main = putStrLn . toAscii
--  -- . modify (LeftMatcher (Const 3)) foldConst
--  . modify (LeftMatcher (Const 1)) foldConst
--  . commuteSum
--  . associateSumRL
--  $ Sum (Const 1) (Sum (Const 2) (Const 3))

--apply (Term -> Term) -> Workspace Term

type AppEff effs = Members '[ Writer [String], State Env ] effs

apply :: AppEff effs => (Term -> Term) -> Eff effs ()
apply f = do
  (Env t) <- get

  tell [toAscii t]
  let t' = f t

  put (Env t')

working = do
  apply foldConst

main = do
  let (t, log) = runApp (Env (Sum (Const 1) (Const 2))) working
  forM_ log $ putStrLn
  putStrLn . toAscii $ t

data Env = Env Term deriving (Show)


runApp :: Env -> Eff '[ Writer [String], State Env] a -> (Term, [String])
runApp env m = do
  let ((_, log), (Env t)) = run . runState env . runWriter $ m

  (t, log)

