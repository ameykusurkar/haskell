module Set3 where

import Data.List

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

-- transpose :: String -> String -> String
