{-# LANGUAGE OverloadedStrings #-}
module Types where

import qualified Data.HashSet           as S
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.Map               as M

import           Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Data.Hashable (Hashable(..))
import Data.Monoid ((<>))

type Identifier = T.Text
type Query = T.Text
type Row = V.Vector Identifier

data Relation = Relation {
  headers  :: V.Vector Identifier,
  elements :: S.HashSet Row
} deriving (Eq, Show)

data Expression =
  Project (V.Vector Identifier) Expression |
  RelationFromEnv Identifier
  deriving (Eq, Show)

type Env = M.Map Identifier Relation

emptyEnv = M.empty

putEnv :: Identifier -> Relation -> Env -> Env
putEnv = M.insert

relationFromEnv :: Identifier -> Env -> Maybe Relation
relationFromEnv = M.lookup

type UserString = T.Text

data EvalError = RelationNotFound Identifier

instance Show EvalError where
  show (RelationNotFound x) = T.unpack ("relation '" <> x <> "' does not exist")


type Eval a = ExceptT EvalError (ReaderT Env Identity) a

instance (Hashable a) => Hashable (V.Vector a) where
  hashWithSalt salt = hashWithSalt salt . V.toList
  {-# INLINE hashWithSalt #-}
