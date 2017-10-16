module Parser where

import Lexer
import Data.List 

data ParseTree = SumDiff Operator ParseTree ParseTree -- a + b or a - b
               | ProdDiv Operator ParseTree ParseTree -- a * b or a / b
               | Unop Operator ParseTree -- +a or -a
               | Assign String ParseTree -- x = ...
               | Num Double -- Numbers
               | Var String -- variables
    deriving Show

-- We can consider the following grammar:
-- Expr = Term [+-] Expr | Ident = Expr | Term
-- Term = Fact [*/] Term | Fact 
-- Fact = Num | Ident | [+-] Fact | ( Expr )

head' :: [Token] -> Token
head' [] = TokEnd
head' (x:xs) = x


preExpr :: [Token] -> (ParseTree, [Token])
preExpr toklst = 
  let (termTree, toklst') = preTerm toklst 
  in 
  case head' toklst' of
    -- Term [+-] Expr
    (TokOp op) | elem op [Plus, Minus] -> 
      let (exprTree, toklst'') = preExpr (tail toklst') 
      in (SumDiff op termTree exprTree, toklst'')
    -- Ident = Expr
    TokAssign -> 
      case termTree of 
        Var str -> 
          let (exprTree, toklst'') = preExpr (tail toklst')
          in (Assign str exprTree, toklst'')
        _ -> error "Only variables can be assigned to"
    -- Term
    _ -> (termTree, toklst')

preTerm :: [Token] -> (ParseTree, [Token])
preTerm toklst =
  let (factTree, toklst') = preFact toklst 
  in 
  case head' toklst' of 
    -- Fact [*/] Term
    (TokOp op) | elem op [Times, Div] -> 
      let (termTree, toklst'') = preTerm (tail toklst') 
      in (ProdDiv op factTree termTree, toklst'')
    -- Fact
    _ -> (factTree, toklst')
                 
preFact :: [Token] -> (ParseTree, [Token])
preFact toklst = 
  case head' toklst of
    -- Num
    (TokNum x) -> (Num x, tail toklst)
    -- Ident
    (TokIdent str) -> (Var str, tail toklst)
    -- [+-] Fact
    (TokOp op) | elem op [Plus, Minus] -> 
      let (factTree, toklst') = preFact (tail toklst) 
      in (Unop op factTree, toklst')
    -- ( Expr ) 
    TokLPar -> 
      let (exprTree, toklst') = preExpr (tail toklst) 
      in 
      case head' toklst' of
        TokRPar -> (exprTree, tail toklst')
        _ -> error "Missing right parenthesis"
    _ -> error $ "Parse Error on token: " ++ show toklst
     
parse :: [Token] -> ParseTree
parse toklst =
  let (exprTree, toklst') = preExpr toklst 
  in  
  if null toklst' then exprTree
  else error $ "Tokens left: " ++ show toklst
