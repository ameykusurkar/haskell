module Set4 where

import Data.Maybe

import Data.List

data Shape = Triangle Float Float Float | Square Float | Circle Float |
             Polygon [(Float, Float)]

area :: Shape -> Float
-- Returns the area of a Shape
area (Square l)               = l * l
area (Circle r)               = pi * r * r
area (Triangle a b c)         = sqrt (s * (s-a) * (s-b) * (s-c))
  where s = (a + b + c) / 2
area (Polygon [p1, p2, p3])   = areaTri p1 p2 p3
area (Polygon (p1:p2:p3:ps))  = (areaTri p1 p2 p3) + area (Polygon (p1:p3:ps))  


areaTri p1 p2 p3 = area (Triangle a b c)
  where a = lineLength p1 p2
        b = lineLength p1 p3
        c = lineLength p2 p3

lineLength :: (Float, Float) -> (Float, Float) -> Float
-- Returns the length of a line
lineLength (x1, y1) (x2, y2)
  = sqrt ( (x1-x2)^2 + (y1-y2)^2 )


type Date = (Int, Int, Int)

age :: Date -> Date -> Int
-- Returns difference in years between 2 dates
age (d, m, y) (d', m', y')
  = if (m, d) <= (m', d') then y' - y else y' - y - 1  


data Possibly a = Failure | Success a
  deriving (Eq, Ord, Show)

tableLookup :: Eq a => Eq b => a -> [(a, b)] -> Possibly b
tableLookup key table
  | lookup key table == Nothing = Failure
  | otherwise = Success (fromJust (lookup key table))


----- University Database questions -----


data Sex       = Male | Female

type Empdata   = (String, Sex, Date, Float)

data Section   = Systems | Software | Theory

type Course    = Int

data SupType   = Administrative | Technical

data StaffType = Teaching Section [Course] | Support SupType

type Ustaff    = (Empdata, StaffType)

type Database  = [Ustaff]

name :: Ustaff -> String
-- Returns the name of the person
name ((n,_,_,_),_) = n

salary :: Ustaff -> Float
-- Returns the salary of the person
salary ((_,_,_,s),_) = s

isSupport :: Ustaff -> Bool
-- Returns True if the person is support staff
isSupport (_, Support _) = True
isSupport _              = False

teaches :: Course -> Ustaff -> Bool
-- Returns true iff person teaches the course
teaches c (_, Teaching _ cs) = elem c cs
teaches _ _                   = False

numSupStaff :: Database -> Int
-- Returns the number of support staff in database
numSupStaff = length . filter (isSupport)

courseTeacher :: Course -> Database -> String
-- Returns the name of the course teacher
courseTeacher c = name . head . filter (teaches c)

salaryBill :: Database -> Float
-- Returns the total salary bill
salaryBill = sum . map salary

-----------------------------------------

{-
data Tree = Leaf | Node Tree Tree
          deriving (Eq,Ord,Show)

makeTrees :: Int -> [Tree]
-- Returns list of all possible trees with n nodes
makeTrees n = makeTrees' n [ [Leaf] ]
  where
  	makeTrees' k ts
  	  | k == 0    = head ts
  	  | otherwise = makeTrees' (k-1) (ts':ts)
  	    where 
  	      ts' = [ Node l r | (t, t') <- zip ts (reverse ts), l <- t, r <- t']
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

build :: [a] -> Tree a
-- Builds a balanced tree from a list
build []  = error "List must be non-empty"
build [x] = Leaf x
build xs  = Node (build l) (build r)
  where (l, r) = split2 xs

split2 :: [a] -> ([a], [a])
-- Splits a list into two equal parts
split2 list = splitAt n list
  where n = (length list) `div` 2

ends :: Tree a -> [a]
-- Converts a tree into list, preserving order
ends (Leaf x)   = [x]
ends (Node l r) = (ends l) ++ (ends r)

swap :: Tree a -> Tree a
-- Swaps sub-trees at each node
swap (Leaf x) = Leaf x
swap (Node l r) = Node (swap r) (swap l)


type Queue a = [a]

nobody = []

arrive :: a -> Queue a -> Queue a
arrive c q = q ++ [c]

first :: Queue a -> a
first (c:cs) = c

serve :: Queue a -> Queue a
serve (c:cs) = cs


data Time = HR24 Int Int | HM Int Int AmPm

instance Eq Time where
  (==) = equaltime

instance Show Time where
  show (HR24 h m)
    | h < 10    = "0" ++ show (h*100 + m) ++ "HRS"
    | otherwise = show (h*100 + m) ++ "HRS"
  show (HM 12 00 PM) = "Midday"
  show (HM 12 00 AM) = "Midnight"
  show (HM h  m  ap) = undefined

data AmPm = AM | PM
  deriving (Eq, Ord, Show)

to24 :: Time -> Time
to24 (HM h m AM)
  | h == 12   = HR24 0 m
  | otherwise = HR24 h m
to24 (HM h m PM)
  | h == 12    = HR24 h m
  | otherwise = HR24 (h+12) m
to24 x           = x

equaltime :: Time -> Time -> Bool
equaltime t t' = h == h' && m == m'
  where 
  	HR24 h m   = to24 t
  	HR24 h' m' = to24 t'



















