module Parser where

import Data.Char

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
--precedence :: Operator -> Precedence
 
--associativity :: Operator -> Associativity

--higherPrecedence :: Operator -> Operator -> Bool

--eqPrecedence :: Operator -> Operator -> Bool

--isRightAssociative :: Operator -> Bool

--supersedes :: Operator -> Operator -> Bool

--stringToInt :: String -> Int

--buildExpr :: String -> Expr

--tokenise :: String -> [Token]

--buildOpApp :: (ExprStack, OpStack) -> (ExprStack, OpStack)

--parse :: [Token] -> (ExprStack, OpStack) -> Expr

