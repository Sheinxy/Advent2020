_countYes acc _ [] = acc
_countYes acc counted (c : l)
  | c `elem` counted = _countYes acc counted l
  | otherwise = _countYes (acc + 1) (c : counted) l
countYes = _countYes 0 []

_sumYes acc [] = acc
_sumYes acc (g : l) = 
    _sumYes (acc + countYes (intercalate "" g)) l
sumYes = _sumYes 0

_countAllYes answered [] = length answered
_countAllYes answered (a : l) =
  _countAllYes [c | c <- answered, c `elem` a] l
countAllYes [] = 0
countAllYes (a : l) = _countAllYes a l

_sumAllYes acc [] = acc
_sumAllYes acc (g : l) = 
    _sumAllYes (acc + countAllYes g) l
sumAllYes = _sumAllYes 0