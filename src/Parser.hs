module Parser where

import           Data.Char
import           Data.Either
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Language           ( haskellStyle )

import qualified Text.Parsec.Expr              as Ex
import qualified Text.Parsec.Token             as Token

import           Syntax

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
 where
  operations = ["->", "\\", "+", "*", "-", "="]
  names      = []
  style      = haskellStyle { Token.reservedOpNames = operations
                            , Token.reservedNames   = names
                            , Token.commentLine     = "#"
                            }

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

contents :: Parser a -> Parser a
contents parser = do
  Token.whiteSpace lexer
  content <- parser
  eof
  return content

natural :: Parser Integer
natural = Token.natural lexer

variable :: Parser Expr
variable = Var <$> identifier

number :: Parser Expr
number = Lit . LInt . fromIntegral <$> natural

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args


term :: Parser Expr
term = parens expr <|> variable <|> number <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return $ foldl1 App es

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
