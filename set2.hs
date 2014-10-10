
addDigit :: Int -> Int -> Int
-- Takes Int x and adds it on to the end of bigInt
addDigit bigInt x
  = bigInt*10 + x

convert :: Float -> Float
-- Converts temperature from degrees C to degrees F
convert c
  = 1.8*c + 32

type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
-- Calculates the distance between 2 points, each represented as a Vertex
distance p1 p2
  = sqrt (diffX^2 + diffY^2)
			where
				diffX = fst p1 - fst p2
				diffY = snd p1 - snd p2

triArea ::  Vertex -> Vertex -> Vertex -> Float
-- Calculates the area of triangle  formed by the 3 given vertices
triArea p1 p2 p3
	= sqrt (s * (s-a) * (s-b) * (s-c))
			where
				s = (a+b+c)/2
				a = distance p1 p3
				b = distance p1 p2
				c = distance p2 p3

fact :: Int -> Int
-- Returns the factorial of an integer
fact n
	| n == 0    = 1
	| otherwise = n * fact (n-1)

perm, choose :: Int -> Int -> Int
-- Calculates the value of nPr
-- Pre: n, r must be non-negative
-- Pre: n >= r
perm n r
	| r == 0    = 1
	| otherwise = perm n (r-1) * (n-r+1)

--choose returns the value of nCr
-- Pre: n, r must be non-negative
-- Pre: n >= r
choose n r
	| n == r    = 1
	| otherwise = choose (n-1) r * n `div` (n-r)

remainder, quotient :: Int -> Int -> Int
-- Gives the remainder after integer division, done using recursion
remainder int divisor
	= if int < divisor then int
		else remainder (int - divisor) divisor

-- Gives the quotient after integer division, done using recursion
quotient int divisor
	| int < divisor = 0
	| otherwise     = quotient (int - divisor) divisor + 1









