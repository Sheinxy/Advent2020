-- Tells if two numbers sum to another number n in a list
sumToN :: [Int] -> Int -> Bool
sumToN [] _ = False
sumToN nbs n = any (==True) [True | i <- nbs, 
                                    j <- nbs,
                                    i /= j,
                                    i + j == n]

-- Finds the first number that isn't the sum of the previous 25 ones
findInvalid :: [Int] -> Int
findInvalid [] = error "findInvalid: empty list"
findInvalid nbs =
  let
    _findInvalid :: [Int] -> [Int] -> Int
    _findInvalid [] _ = error "findInvalid: empty 25"
    _findInvalid _ [] = error "findInvalid: not found"
    _findInvalid (f : t) (n : l)
      | sumToN (f : t) n = _findInvalid (t ++ [n]) l 
      | otherwise = n 
  in 
  _findInvalid (take 25 nbs) (drop 25 nbs)

-- Sums the extremes of a continuous set of number summing to N
contiguousSumN :: [Int] -> Int -> Int 
contiguousSumN [] _ = error "contiguousSumN: empty list"
contiguousSumN [_] _ = error "contiguousSumN: list too small"
contiguousSumN l n =
  let
    _contiguousSumN :: [Int] -> [Int] -> Int -> Int
    _contiguousSumN _ [] _ = error "contiguousSumN: not found"
    _contiguousSumN c (e : l) acc
      | acc + e == n = minimum (e : c) + maximum (e : c)
      | acc + e > n = 
        _contiguousSumN (init c) (e : l) (acc - last c)
      | otherwise = _contiguousSumN (e : c) l (acc + e)
  in
  _contiguousSumN [] l 0


-- Reads the input file and returns a Program
readInput :: IO [Int]
readInput = do
  input <- readFile "days/inputs/day9.txt"
  return ([read l :: Int | l <- lines input])