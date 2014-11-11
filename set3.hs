module Set3 where

import Data.List
import Data.Char

precedes :: String -> String -> Bool
-- Returns True if first string is lexicographically less than second
precedes (s:ss) (s':ss')
  = s < s' || precedes ss ss'
precedes _  []
  = False
precedes [] _
  = True


pos :: Eq a => a -> [a] -> Int
-- Returns the position of an int in a list of ints
pos i (n:ns)
  | i == n    = 0
  | otherwise = 1 + pos i ns 


twoSame :: [Int] -> Bool
-- Returns True iff there are duplicates in the list
twoSame []     = False
twoSame (r:rs) = elem r rs || twoSame rs


primeFactors :: Int -> [Int]
-- Returns a list containing all the prime factors of the integer
-- Pre: x >= 1
primeFactors n = divFactors n 2 []
  where
    divFactors :: Int -> Int -> [Int] -> [Int]
    divFactors k d xs
      | fromIntegral d > sqrt (fromIntegral k) = k:xs
      | mod k d == 0                           = divFactors (div k d) d (d:xs)
      | otherwise                              = divFactors k (d+1+(mod d 2)) xs


hcf, lcm :: Int -> Int -> Int
-- hcf: highest common factor, lcm: lowest common multiple
hcf a b = a `div` product (primeFactors a \\ primeFactors b)
lcm a b = b * product (primeFactors a \\ primeFactors b)


right :: a -> [a] -> [a]
-- Adds and element to the end of a list
right y []     = [y]
right y (x:xs) = x : (right y xs) 


backwards :: [a] -> [a]
-- Returns the list in reverse
backwards xs = backwards' xs []
  where 
    backwards' []     acc = acc
    backwards' (x:xs) acc = backwards' xs (x:acc)


transpose :: String -> String -> String -> String
-- Transposes a string based on a transposition defined by 2 strings
-- Pre: All 3 strings are of the same length
-- Set3.transpose defined because of name clashing with Prelude
transpose str from (t:ts)
  = ( str !! (pos t from) ) : (Set3.transpose str from ts)
transpose str from []
  = []


substring :: String -> String -> Bool
-- Returns True is the first string is a substring of the second
substring test (x:xs)
 = and ( zipWith (==) test (x:xs) ) || substring test xs
substring test []
 = False


nextWord :: String -> ( String, String )
-- Returns a tuple of the next word and the rest of the string 
nextWord [] = ([], [])
nextWord (first:rest)
  | isSpace first = ([], rest)
  | otherwise     = (first : word, string)
    where
      (word, string) = nextWord rest


splitUp :: String -> [String]
-- Returns list of words in the string
splitUp []  = []
splitUp (c:cs) 
  | isSpace c = splitUp cs
  | otherwise = first : splitUp rest
  where
    (first, rest) = nextWord (c:cs)


merge2 :: [Int] -> [Int] -> [Int]
-- Merges 2 ordered lists
merge2 xs [] = xs
merge2 [] ys = ys
merge2 (x:xs) (y:ys)
  | x < y    = x : ( merge2 xs (y:ys) )
  |otherwise = y : ( merge2 (x:xs) ys )


timesTable :: Int -> Int -> [String]
-- Returns a list of timestables upto a*b
timesTable a b = [disp x y | x <- [1..a], y <- [1..b] ]
  where
    disp x y = show x ++ " times " ++ show y ++ " is " ++ show (x*y)


qsort :: [Int] -> [Int]
-- Sorts a list of ints
qsort []     = [] 
qsort (i:is) = (qsort lesser) ++ [i] ++ (qsort greater)
  where 
    lesser  = [ x | x <- is, x <= i ]
    greater = [ y | y <- is, y > i  ]  


perms :: Eq a => [a] -> [[a]]
-- Returns a list of all the permutations of a list
perms list = [ list \\ [l] | l <- list ]





