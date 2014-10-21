ints :: Int -> [Int] 
ints n = ints' n []
  where
    ints' :: Int -> [Int] -> [Int]
    ints' k xs
      | k == 0 = xs
      | otherwise = ints' (k-1) (k:xs)
