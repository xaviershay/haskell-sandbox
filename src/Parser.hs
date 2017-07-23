module Parser where

import qualified Data.HashSet           as S
import qualified Data.Text              as T
import qualified Data.Vector            as V

import           Text.Parsec            hiding (many, optional, (<|>))
import           Text.Parsec.Expr
import           Control.Monad.Identity (Identity)

import Types

type SqlParser = ParsecT T.Text (Maybe Expression) Identity Expression
type ParseResult = Either UserString Expression


parseSql :: UserString -> ParseResult
parseSql input = transformError $ runParser sqlExpr () (T.unpack input) input
  where
    transformError = either (Left . T.pack . show) Right

sqlExpr = select

select = do
  string "SELECT "
  columns <- many1 alphaNum
  string " FROM "
  relationName <- many1 alphaNum

  return $ Project (V.singleton $ T.pack columns) (RelationFromEnv $ T.pack relationName)
