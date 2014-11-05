module Mastermind where

import Data.List

data Colour = R | B | G deriving (Eq, Ord, Show)

type Colours = [Colour] 

type Score = (Int, Int)

type Result = (Colours, Score)            

colour :: Int -> Colour
-- Returns the Colour associated with the integers 0, 1, 2
colour 0 = R
colour 1 = B
colour 2 = G
colour _ = error "Input must be 0, 1 or 2."


base3, base3' :: Int -> Int -> [Int]
base3 x = reverse . base3' x
base3' x n
  | x < 3     = x : ( take (n-1) (repeat 0) )
  | otherwise = (mod x 3) : ( base3' (div x 3) (n-1) )


blacks :: Colours -> Colours -> Int
-- Returns the number of colours in the correct place,
-- for a guess compared to a secret code
blacks gs = length.filter(id).zipWith (==) gs

score :: Colours -> Colours -> Result
-- Returns the score of a guess compared to the secret code,
-- in the format ( guess, (blacks, whites) )
score g s = ( g, (b, w) )
  where 	
    b = blacks g s
    w = length s - length (g \\ s) - b

allGuesses :: Int -> [Colours]
-- Returns a list of all possible guesses of length n
allGuesses n = map (map colour . (`base3` n)) [0..(3^n-1)]


consistent :: Colours -> [Result] -> Bool
-- Returns true if the guess matches the list of results it would give,
-- if it was assumed as the correct secret code
consistent g rs = and [score g' g == r | r@(g', s) <- rs]

strike :: [Colours] -> [Result] -> [Colours]
-- Gives a list of possible answers from given a list of guesses
-- based on a list of results
strike gs rs = filter (`consistent` rs) gs 

----------------------------------------------------

sampleResults :: [Result]
sampleResults = [([R,B,B],(1,0)), ([R,G,G],(2,0))]

