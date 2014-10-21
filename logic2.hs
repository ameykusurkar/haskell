
type Cord = (Int, Int)

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




