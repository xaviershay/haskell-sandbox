module Parser where

import qualified Data.HashSet           as S
import qualified Data.Text              as T

import           Text.Parsec            hiding (many, optional, (<|>))
import           Text.Parsec.Expr
import           Control.Monad.Identity (Identity)

type Identifier = T.Text
type Query = T.Text
type Row = [Identifier]

data Relation = Relation {
  headers  :: [Identifier],
  elements :: S.HashSet Row
} deriving (Eq, Show)

data Expression =
  Select [Identifier] Expression |
  RelationFromEnv Identifier
  deriving (Eq, Show)

type SqlParser = ParsecT T.Text (Maybe Expression) Identity Expression
type ParseResult = Either ParseError Expression

--parseSql :: Query -> ParseResult
parseSql input = runParser sqlExpr () (T.unpack input) input

sqlExpr = select

select = do
  string "SELECT "
  columns <- many1 alphaNum
  string " FROM "
  relationName <- many1 alphaNum

  return $ Select [T.pack columns] (RelationFromEnv $ T.pack relationName)
