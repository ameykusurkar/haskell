module Parser where

import Data.Char

import Data.Maybe

type Operator = Char

data Expr = Num Int | Var String | Op Operator | App Expr Expr Expr
            deriving (Eq,Ord,Show)

type Token = Expr

data Associativity = L | N | R
                     deriving (Eq,Ord,Show)

ops :: [Operator]
ops = "+-*/^()$"

type Precedence = Int

opTable :: [(Operator, (Precedence, Associativity))]
opTable = [('$',(0,N)), ('(',(1,N)), ('+',(6,L)), ('-',(6,L)), 
           ('*',(7,L)), ('/',(7,L)), ('^',(8,R)), (')',(1,N))]

type ExprStack = [Expr]

type OpStack = [Operator]

showExpr :: Expr -> String
showExpr (Num n) 
  = show n 
showExpr (Var s) 
  = s
showExpr (Op c)  
  = [c]
showExpr (App op e e') 
  = "(" ++ showExpr e ++ showExpr op ++ showExpr e' ++ ")"

--------------------------------------------

--
-- Assume throughout that all function arguments are valid, for example:
--   All input expressions are well-formed
--   All Operators (Chars) are members of 'ops' above
--   The stacks passed to buildOpApp and parse constitute valid `state'
--   with respect to the Shunting Yard algorithm
--


-------------------------------------------------------------------
precedence :: Operator -> Precedence
-- Returns the Precedence of an operator
precedence op = p
  where (p, _) = fromJust (lookup op opTable)
 
associativity :: Operator -> Associativity
-- Returns the Associativity of an operator
associativity op = a
  where (_, a) = fromJust (lookup op opTable)

higherPrecedence :: Operator -> Operator -> Bool
higherPrecedence op1 op2
  = precedence op1 > precedence op2

eqPrecedence :: Operator -> Operator -> Bool
eqPrecedence op1 op2
  = precedence op1 == precedence op2

isRightAssociative :: Operator -> Bool
isRightAssociative op
  = associativity op == R

supersedes :: Operator -> Operator -> Bool
supersedes op1 op2
  = higherPrecedence op1 op2 || eqPrecedence op1 op2 && isRightAssociative op1


stringToInt :: String -> Int
stringToInt = foldl1 (\a b -> a*10 + b) . map digitToInt

buildExpr :: String -> Expr
buildExpr s
  = parse (tokenise s) ([], ['$'])

tokenise :: String -> [Token]
tokenise [] = []
tokenise (s:ss)
  | isSpace s  = tokenise ss
  | elem s ops = (Op s):(tokenise ss)
  | otherwise  = expr:(tokenise rest)
    where 
      ( str@(s':ss'), rest ) = break (not.isAlphaNum) (s:ss)
      expr = if isDigit s' then Num (stringToInt str) else Var str

buildOpApp :: (ExprStack, OpStack) -> (ExprStack, OpStack)
buildOpApp ((n : n' : es), (o : os))
  = (App (Op o) n' n : es, os)

parse :: [Token] -> (ExprStack, OpStack) -> Expr
parse [] ([e], _)     = e
parse [] stack        = parse [] (buildOpApp stack)
parse ((Op op):ts) ( es, (o:os) )
  | supersedes op o = parse ts ( es, (op:o:os) )
  | otherwise       = parse ts ( buildOpApp (es, (op:o:os)) )
parse (t:ts) (es, os) = parse ts ( (t:es), os )














