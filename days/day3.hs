_countTrees acc (x, y) (dx, dy) l
  | y >= length l = acc
  | (l !! y) !! (mod x (length (l !! y))) == '#' =
      _countTrees (acc + 1) (x + dx, y + dy) (dx, dy) l
  | otherwise = 
      _countTrees acc (x + dx, y + dy) (dx, dy) l
countTrees = _countTrees 0 (0, 0)

_countTreesSlopes acc [] _ = acc
_countTreesSlopes acc (slope : slopes) l =
  _countTreesSlopes (acc * countTrees slope l) slopes l
countTreesSlopes = _countTreesSlopes 1
