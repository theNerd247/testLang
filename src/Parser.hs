{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}


module Parser where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle, emptyDef)
import Syntax
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

parseExpr :: String → Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

contents :: Parser a → Parser a
contents p = Tok.whiteSpace lexer *> p <* eof

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable term

opTable :: Ex.OperatorTable String () Identity Expr
opTable = 
  [ [ prefixOp "succ"   Succ
    , prefixOp "pred"   Pred
    , prefixOp "iszero" IsZero
    ]
  ]

term :: Parser Expr
term = 
      true
  <|> false
  <|> zero
  <|> ifelse
  <|> parens expr 

true :: Parser Expr
true = reserved "true" *> pure Tr

false :: Parser Expr
false = reserved "false" *> pure Fl

zero :: Parser Expr
zero = reserved "0" *> pure Zero

ifelse :: Parser Expr
ifelse = 
      reserved "if" 
  *>  (pure If)
  <*> expr
  <*  reservedOp "then"
  <*> expr
  <*  reservedOp "else"
  <*> expr

langDef :: Tok.LanguageDef ()
langDef = emptyDef
  { Tok.commentLine    = "--"
  , Tok.nestedComments = False
  , Tok.identStart     = letter
  , Tok.identLetter    = alphaNum <|> oneOf "_'"
  , Tok.caseSensitive  = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a → Parser a
parens     = Tok.parens lexer

reserved :: String → Parser ()
reserved = Tok.reserved lexer

semisep :: Parser a → Parser [a]
semisep = Tok.semiSep lexer

reservedOp :: String → Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String → (a → a) → Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix $ reservedOp s >> return f

