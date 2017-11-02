module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [">", ">=", "<", "<=", "==", "!=", "+", "-", "*", "/", "=", ":="]
    names = ["if", "then", "else", "while", "let", "in", "print"]
    style = emptyDef {
              Tok.reservedOpNames = ops,
              Tok.reservedNames = names
            }

integer :: Parser Integer
integer = Tok.integer lexer

identifier :: Parser String
identifier = Tok.identifier lexer

-- parens p parses p enclosed in parenthesis --
parens :: Parser a -> Parser a 
parens = Tok.parens lexer

-- braces p parses p enclosed in braces ('{' and '}') --
braces :: Parser a -> Parser a 
braces = Tok.braces lexer 

-- semiSep1 p parses one or more occurrences of p separated by ";" --
semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
