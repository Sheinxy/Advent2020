import Data.List

_findDicho _ _ [] = error "Invalid string"
_findDicho (low, upp) (c_l, c_u) (c : [])
  | c == c_l = low
  | c == c_u = upp
  | otherwise = error "Invalid string"
_findDicho (low, upp) (c_l, c_u) (c : l)
  | c == c_l= 
      _findDicho (low, div (upp + low) 2) (c_l, c_u) l
  | c == c_u && mod (upp + low) 2 == 0 = 
      _findDicho (div (upp + low) 2, upp) (c_l, c_u) l
  | c == c_u = 
      _findDicho (1 + div (upp + low) 2, upp) (c_l, c_u) l
  | otherwise = error "Invalid string"
findRow = _findDicho (0, 127) ('F', 'B')
findCol = _findDicho (0, 7) ('L', 'R')

seatId seat = 
  8 * findRow (take 7 seat) + findCol (drop 7 seat)

_findBestSeat best [] = best
_findBestSeat best (s : l) =
  _findBestSeat (max best (seatId s)) l
findBestSeat = _findBestSeat 0

_allIds acc [] = sort acc
_allIds acc (s : l) =
  _allIds ((seatId s) : acc) l
allIds = _allIds []

_findMissingId [] = error "No Missing ID"
_findMissingId (_ : []) = error "No Missing ID"
_findMissingId (a : b : l)
  | a + 1 /= b = a + 1
  | otherwise = findMissingId (b : l)
findMissingId input = _findMissingId (allIds input)