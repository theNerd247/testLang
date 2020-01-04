{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Parsec
import Text.Parsec.Language (haskellStyle, emptyDef)
import qualified Syntax as S
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

langDef :: Tok.LanguageDef ()
langDef = emptyDef
  { Tok.commentLine    = "--"
  , Tok.nestedComments = False
  , Tok.identStart     = letter
  , Tok.identLetter    = alphaNum <|> oneOf "_'"
  , Tok.reservedNames  = [ "add", "mul" ]
  }

lexer      = Tok.makeTokenParser langDef
parens     = Tok.parens lexer
reserved   = Tok.reserved lexer
natural    = Tok.natural lexer
num        = (S.num . fromInteger) <$> natural
add        = reserved "add" *> (pure S.add) <*> pexpr <*> pexpr
expr       = num <|> add <|> pexpr
pexpr      = parens expr
whitespace = Tok.whiteSpace lexer
contents p = whitespace *> p <* eof

parseExpr = parse (contents expr) "<stdin>"
