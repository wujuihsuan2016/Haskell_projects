module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Ast
import Lexer 

-- rexp   -> rexp relop expr | expr
-- expr   -> expr addop term | term
-- term   -> term mulop factor | factor
-- factor -> var | const | (expr) 
-- var    -> identifier
-- const  -> integer
-- addop  -> + | -
-- mulop  -> * | /
-- relop  -> > | >= | < | <= | == | != 

const' :: Parser Exp
const' = integer >>= return . Const

var :: Parser Exp
var = identifier >>= return . Var 

relop :: Parser (Exp -> Exp -> Exp)
relop =  try (reservedOp ">=" >> return (Binop Gequal))
     <|> try (reservedOp ">"  >> return (Binop Greater))
     <|> try (reservedOp "<=" >> return (Binop Lequal))
     <|> try (reservedOp "<"  >> return (Binop Less))
     <|> try (reservedOp "!=" >> return (Binop Unequal))
     <|> try (reservedOp "==" >> return (Binop Equal))

addop :: Parser (Exp -> Exp -> Exp)
addop =  (reservedOp "+" >> return (Binop Plus))
     <|> (reservedOp "-" >> return (Binop Minus))

mulop :: Parser (Exp -> Exp -> Exp)
mulop =  (reservedOp "*" >> return (Binop Times))
     <|> (reservedOp "/" >> return (Binop Divide))

factor :: Parser Exp
factor =  try var 
      <|> try const'
      <|> try (parens rexp)

term :: Parser Exp 
term = factor `chainl1` mulop

expr :: Parser Exp
expr = term `chainl1` addop

rexp :: Parser Exp
rexp = expr `chainl1` relop

assign :: Parser Com
assign = identifier >>= \v -> reservedOp ":=" >> rexp >>= \e -> return (Assign v e)  

seq' :: Parser Com
seq' = (braces . semiSep1) com >>= return . Seq 

cond :: Parser Com 
cond = reserved "if" >> rexp >>= \e -> reserved "then" >> com >>= \c1 -> reserved "else" >> com >>= \c2 -> return (Cond e c1 c2)

while :: Parser Com
while = reserved "while" >> rexp >>= \e -> reserved "do" >> com >>= \c -> return (While e c)

let' :: Parser Com
let' = reserved "let" >> identifier >>= \v -> reserved "=" >> rexp >>= \e -> reserved "in" >> com >>= \c -> return (Let v e c)

print' :: Parser Com
print' = reserved "print" >> rexp >>= \e -> return $ Print e

com :: Parser Com
com =  try assign
   <|> try seq'
   <|> try cond
   <|> try while
   <|> try let'
   <|> try print'

program :: Parser Com
program = com >>= \c -> eof >> return c        

parseProgram :: String -> Either ParseError Com
parseProgram s = parse program "<stdin>" s 
