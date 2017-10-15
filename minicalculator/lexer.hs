module Lexer (tokenize) where

import Data.Char
import Data.List

data Expr
data Operator = Plus | Minus | Times | Div deriving (Show,Eq)
data Token = TokOp Operator  
           | TokIdent String
           | TokNum Double
           | TokLPar
           | TokRPar
           | TokAssign
           | TokEnd
    deriving (Show,Eq)

opToChar :: Operator -> Char
opToChar op 
    | op == Plus  = '+'
    | op == Minus = '-'
    | op == Times = '*'
    | op == Div   = '/' 

charToOp :: Char -> Operator
charToOp c 
    | c == '+' = Plus
    | c == '-' = Minus
    | c == '*' = Times
    | c == '/' = Div

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) 
    | elem c "+-*/" = TokOp (charToOp c) : tokenize cs
    | isDigit c = let (s1, s2) = prefixNum cs in TokNum (read (c:s1)) : tokenize s2
    | isAlpha c = let (s1, s2) = prefixAlphaNum cs in TokIdent (c:s1) : tokenize s2 
    | isSpace c = tokenize cs
    | c == '(' = TokLPar : tokenize cs
    | c == ')' = TokRPar : tokenize cs
    | c == '=' = TokEq : tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

prefixAlphaNum :: String -> (String, String)
prefixAlphaNum = span isAlphaNum

prefixNum :: String -> (String, String)
prefixNum = span isDigit
