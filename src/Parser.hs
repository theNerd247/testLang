{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}


module Parser where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle, emptyDef)
import Syntax
import Data.Bifunctor (first)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

parseExpr :: String → Either String Expr
parseExpr = first show . parse (contents expr) "<stdin>"

contents :: Parser a → Parser a
contents p = whiteSpace *> p <* eof

expr :: Parser Expr
expr = foldr1 App <$> (many1 term)

term :: Parser Expr
term = 
      lit
  <|> var
  <|> lam
  <|> letExpr
  <|> parens expr

var :: Parser Expr
var = Var <$> ident

lit :: Parser Expr
lit = 
      Lit <$> true  
  <|> Lit <$> false
  <|> Lit <$> int

true :: Parser Lit
true = reserved "True" *> (pure $ LBool True)

false :: Parser Lit
false = reserved "False" *> (pure $ LBool False)

int :: Parser Lit
int = LInt <$> integer

letExpr :: Parser Expr
letExpr = do
  reserved "let" 
  n <- ident
  reserved "="
  e <- expr
  reserved "in"
  t <- expr
  return $ App (Lam n t) e


lam :: Parser Expr
lam = 
      reservedOp "\\"
  *>  pure Lam 
  <*> ident
  <*  reservedOp "->" 
  <*> expr

langDef :: Tok.LanguageDef ()
langDef = emptyDef
  { Tok.commentLine     = "--"
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.caseSensitive   = True
  , Tok.reservedOpNames = [ "\\" , "->" ]
  , Tok.reservedNames   = [ "True" , "False", "let", "in", "=" ]
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a → Parser a
parens     = Tok.parens lexer

ident :: Parser Name
ident = Tok.identifier lexer

integer :: Parser Int
integer = fromInteger <$> (Tok.integer lexer)

reserved :: String → Parser ()
reserved = Tok.reserved lexer

semisep :: Parser a → Parser [a]
semisep = Tok.semiSep lexer

reservedOp :: String → Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String → (a → a) → Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix $ reservedOp s >> return f

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer
