
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
fib n = fib' 0 1 0
	where
		fib' :: Int -> Int -> Int -> Int
		fib' x x1 k
			| k == n    = x
			| otherwise = fib' x1 (x+x1) (k+1)

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
	 
binary :: Int -> Int
-- Returns a binary representation of an integer in decimal representation
binary d
	| d < 2     = d
	| otherwise = (binary (div d 2))*10 + mod d 2

newbase :: Int -> Int -> Int
-- Returns an integer in the representation of a new base
-- Pre: base <= 10
newbase num base
  | num < base = num
  | otherwise  = (newbase (div num base) base)*10 + mod num base

add2, larger :: Int -> Int -> Int
-- Returns the sum of 2 non-negative numbers
-- using only pred and succ
add2 x y
  | x == 0    = y
  | otherwise = add2 (pred x) (succ y)

-- Returns the larger of 2 positive integers using recursion
larger x y
  | x == 0    = y
  | y == 0    = x
  | otherwise = larger (pred x) (pred y) + 1

chop :: Int -> (Int, Int)
-- Returns a tuple which contains the first (n-1) digits and the last digit
-- of an integer separately
chop n
  | n < 10    = (0, n)
  | otherwise = (a + 1, b)
		where
			(a, b) = chop (n-10)

concatenate :: Int -> Int -> Int
-- Concatenates the digits of 2 non-zero integers
-- INCOMPLETE
concatenate x y
  | y == 0    = x
  | otherwise = (concatenate x a)*10 + b
		where
			(a, b) = chop y 



