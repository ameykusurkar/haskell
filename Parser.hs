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
  where (p, a) = fromJust (lookup op opTable)
 
associativity :: Operator -> Associativity
-- Returns the Associativity of an operator
associativity op = a
  where (p, a) = fromJust (lookup op opTable)

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
stringToInt str = strToInt (reverse str)
  where
    -- ord '0' = 48
    strToInt []     = 0
    strToInt (s:ss) = (ord s - 48) + (strToInt ss) * 10

--buildExpr :: String -> Expr

--tokenise :: String -> [Token]

--buildOpApp :: (ExprStack, OpStack) -> (ExprStack, OpStack)

--parse :: [Token] -> (ExprStack, OpStack) -> Expr

