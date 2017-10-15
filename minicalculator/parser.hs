module Parser where

import Lexer

data ParseTree = SumDiff Operator ParseTree ParseTree -- a + b or a - b
               | ProdDiv Operator ParseTree ParseTree -- a * b or a / b
               | Unop Operator Tree -- +a or -a
               | Assign String Tree -- x = ...
               | Num Double -- Numbers
               | Var String -- variables
    deriving Show


