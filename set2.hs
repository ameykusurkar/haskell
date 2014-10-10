
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

fib :: Int -> Int
-- Returns the nth Fibonacci number
fib n
	| n <= 1 = n
	| otherwise = fib (n-1) + fib (n-2)

testFib :: Int -> [Int]
-- Returns a list containing the first n Fibonacci numbers
testFib n
	= [fib x | x <- [0..n]]

gRatio :: Int -> Float -> Float
-- Returns the an approximation of the Golden Ratio
-- which satisfies a threshhold value e
gRatio n e
	| abs (fN1fN - fN2fN1) < e = fN2fN1
	| otherwise                = gRatio (n+1) e
		where
			fN1fN  = fromIntegral (fib (n+1)) / fromIntegral (fib n)
			fN2fN1 = fromIntegral (fib (n+2)) / fromIntegral (fib (n+1))	 







