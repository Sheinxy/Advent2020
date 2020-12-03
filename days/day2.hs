_countChar acc _ [] = acc 
_countChar acc c (e : l) = 
  _countChar (acc + (if e == c then 1 else 0)) c (l)
countChar c s = _countChar 0 c s

_countPasswords _ acc [] = acc
_countPasswords f acc (e : l) = 
  _countPasswords f (acc + (if f e then 1 else 0)) l


isValidPassword1 ((lower, higher), c, s) = 
  let count = countChar c s
  in (count >= lower && count <= higher)

countPasswords1 = _countPasswords isValidPassword1 0


isValidPassword2 ((first, second), c, s) = 
  (length s >= first  && length s >= second) &&
  ((s !! (first - 1) == c) || (s !! (second - 1) == c)) &&
  ((s !! (first - 1)) /= (s !! (second - 1)))

countPasswords2 = _countPasswords isValidPassword2 0