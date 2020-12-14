findSumN n a [] = -1
findSumN n a (e : l)
  | e + a == n = e
  | otherwise = findSumN n a l
solveSum2020 = findSumN 2020

find2SumN n [] = (-1, -1)
find2SumN n (e : l) = 
  let a = (findSumN n e l) in
  if a == -1 then find2SumN n l
  else (e, a)
solve2Sum2020 l = 
  let (a, b) = (find2SumN 2020 l) in a * b

find3SumN n [] = (-1, -1, -1)
find3SumN n (e : l) =
  let (a, b) = (find2SumN (n - e) (e : l)) in
  if (a, b) == (-1, -1) then find3SumN n l
  else (a, b, e)
solve3Sum2020 l =
  let (a, b, c) = (find3SumN 2020 l) in a * b * c

-- I found this solution during day 9
sumTo2020 :: [Int] -> Int
sumTo2020 l = product $ head [[i, j] | i <- l,
                                       j <- l,
                                       i /= j,
                                       i + j == 2020]