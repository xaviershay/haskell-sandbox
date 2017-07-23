{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.HashSet           as S
import Data.Witherable (catMaybes)
import Data.Monoid ((<>))

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Vector as V

import Types

runEval env q = runIdentity $ runReaderT (runExceptT (eval q)) env

eval :: Expression -> Eval Relation
eval (RelationFromEnv name) = do
  env <- ask
  case relationFromEnv name env of
    Just x -> return x
    Nothing -> throwError ("relation '" <> name <> "' does not exist")

eval (Project columns x) = do
  r@Relation { headers = prevHeaders, elements = prevElements } <- eval x

  let indices = catMaybes $ V.map (\x -> V.elemIndex x prevHeaders) columns
  -- TODO: Use safe lookup
  let f = \row -> V.map (\i -> row V.! i) indices

  return $ Relation {
    headers = columns,
    elements = S.map f prevElements
  }

