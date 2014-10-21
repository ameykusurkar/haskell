
type Cord = (Int, Int)
type CordSet = [(Cord, Cord, Cord, Cord, Cord, Cord)]

nextTo, sees, leftOf, above :: Cord -> Cord -> Bool

-- Returns True if a is adjacent to b
nextTo (ax, ay) (bx, by)
  | ay == by  = abs (ax-bx) == 1
  | ax == bx  = abs (ay-by) == 1
  | otherwise = False

-- Returns True if a and b are in the same row or column
sees (ax, ay) (bx, by)
  = (ax == bx) || (ay == by)

-- Returns True if a is in a column to the left of the column of b
leftOf (ax, ay) (bx, by)
  = ax < bx

-- Returns True if a is in row above the row of b
above (ax, ay) (bx, by)
  = ay < by

compute :: CordSet
compute =  comp all6
  where
    all6 = [ (a, b, c, d, e, f) | a <- allPoints, b <- allPoints, c <- allPoints, d <- allPoints, e <- allPoints, f <- allPoints, (a/=b) && (a/=c) && (a/=d) && (a/=e) && (a/=f) && (b/=c) && (b/=d) && (b/=e) && (b/=f) && (c/=d) && (c/=e) && (c/=f) && (d/=e) && (d/=f) && (e/=f) ] 
    allPoints = [(x, y) | x <- [0..2], y <- [0..2] ]
    -- uniquePoints = (a/=b) && (a/=c) && (a/=d) && (a/=e) && (a/=f) && (b/=c) && (b/=d) && (b/=e) && (b/=f) && (c/=d) && (c/=e) && (c/=f) && (d/=e) && (d/=f) && (e/=f) 
   
    comp :: CordSet -> CordSet
    comp set = f5 (f4 (f3 (f2 (f1 set))))

f1, f2, f3, f4, f5 :: CordSet -> CordSet

f1 points = [ (a, b, c, d, e, f) | (a, b, c, d, e, f) <- points, ((sees a b) && (sees b c)) || ((not (sees a b)) && (not (sees b c))) ]

f2 points = [ (a, b, c, d, e, f) | (a, b, c, d, e, f) <- points, (nextTo b d) || (nextTo b e) ]

f3 points = [ (a, b, c, d, e, f) | (a, b, c, d, e, f) <- points, not ((leftOf a f) && (above f a)) ]

f4 points = [ (a, b, c, d, e, f) | (a, b, c, d, e, f) <- points, not (sees a e) ]

f5 points = [ (a, b, c, d, e, f) | (a, b, c, d, e, f) <- points, (above b e) /= (nextTo b c) ]

-- cordToList :: (Cord, Cord, Cord, Cord, Cord, Cord) -> [Cord]
-- cordToList (a, b, c, d, e, f) = [a, b, c, d, e, f]







