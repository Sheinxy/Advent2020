import Data.Char


_isInDict acc validator _ [] = acc
_isInDict True validator _ _ = True
_isInDict acc validator key ((k, val) : d) = 
  _isInDict (k == key && validator k val) validator key d
isValidInDict = _isInDict False
isInDict = isValidInDict (\a b -> True)


_areInDict acc v [] _ = acc
_areInDict False v _ _ = False
_areInDict acc v (key : keys) d =
  _areInDict (isValidInDict v key d) v keys d
areValidInDict = _areInDict True
areInDict = areValidInDict (\a b -> True)

_countValidPassport c v _ [] = c
_countValidPassport c v k (pass : l)
  | not(areValidInDict v k pass) = 
      _countValidPassport c v k l
  | otherwise = _countValidPassport (c + 1) v k l
countValidPassport1 = _countValidPassport 0 (\a b -> True)
solvePart1 = 
  countValidPassport1 ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


_stringToInt n [] = n
_stringToInt n (c : s)
  | '0' <= c && c <= '9' = 
      _stringToInt (n * 10 + ord c - ord '0') s
  | otherwise = -1
stringToInt = _stringToInt 0

validateHeight n u
  | u == "cm" = (let d = stringToInt n in 
      193 >= d && d >= 150)
  | u == "in" = (let d = stringToInt n in
      76 >= d && d >= 59)
  | otherwise = False

_validateHair acc [] = acc
_validateHair False _ = False
_validateHair acc (c : s) =
  _validateHair ('a' <= c && c <= 'z' || '0' <= c && c <= '9') s
validateHair = _validateHair True

validateKey key val
  | key == "byr" = (let d = stringToInt val in
      2002 >= d && d >= 1920)
  | key == "iyr" = (let d = stringToInt val in 
      2020 >= d && d >= 2010)
  | key == "eyr" = (let d = stringToInt val in 
      2030 >= d && d >= 2020)
  | key == "hgt" = 
    (validateHeight (take (length val - 2) val) (drop (length val - 2) val))
  | key == "hcl" = 
      take 1 val == "#" && validateHair (drop 1 val) && length (drop 1 val) == 6
  | key == "ecl" = 
      any (\n -> n == val) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  | key == "pid" = 
    length val == 9 && stringToInt val /= -1
  | otherwise = True


countValidPassport2 = _countValidPassport 0 validateKey
solvePart2 = 
  countValidPassport2 ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]