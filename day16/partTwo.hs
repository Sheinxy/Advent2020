import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

-- Type definition
type Range = (Int, Int)
type Rules = M.Map String (Range, Range)
type Probability = M.Map Int (M.Map String Int)
type Fields = M.Map String Int
type Ticket = [Int]

-- Checks if a value falls within two ranges
inRange :: Int -> (Range, Range) -> Bool
inRange i ((r1, r2), (r3, r4)) =
  (r1 <= i && i <= r2) || (r3 <= i && i <= r4)

-- Checks if a ticket is valid
isValid :: Rules -> Ticket -> Bool
isValid r t =
  let
    _existRange :: Int -> [(Range, Range)] -> Bool
    _existRange _ [] = False
    _existRange i (r :l)
      | inRange i r = True
      | otherwise = _existRange i l
    _isValid :: Ticket -> [(Range, Range)] -> Bool
    _isValid [] _ = True
    _isValid (tk : l) rs
      | _existRange tk rs = _isValid l rs
      | otherwise = False
  in
  _isValid t $ M.elems r

-- Discard the invalid tickets 
removeInvalid :: Rules -> [Ticket] -> [Ticket]
removeInvalid r t =
  let
    _removeInvalid :: [Ticket] -> [Ticket] -> [Ticket]
    _removeInvalid a [] = a
    _removeInvalid a (tk : l)
      | isValid r tk = _removeInvalid (tk : a) l
      | otherwise = _removeInvalid a l
  in
  _removeInvalid [] t

-- Computes the probability of each field for a ticket
ticketProb :: Ticket -> Rules -> Probability -> Probability
ticketProb t r p =
  let
    f :: Maybe Int -> Maybe Int
    f a = maybe (Just 1) (\x -> Just (1 + x)) a
    _rankProb :: Int -> Ticket -> String -> (Range, Range) -> Probability -> Probability
    _rankProb _ [] _ _ p = p
    _rankProb i (tk : l) n r p
      | M.notMember i p && inRange tk r = _rankProb (i + 1) l n r (M.insert i (M.fromList [(n, 0)]) p)
      | inRange tk r =
        _rankProb (i + 1) l n r (M.insert i (M.alter f n (p M.! i)) p)
      | otherwise = _rankProb (i + 1) l n r p
    _ticketProb :: [(String, (Range, Range))] -> Probability -> Probability
    _ticketProb [] p = p
    _ticketProb ((s, r) : l) p =
      _ticketProb l $ _rankProb 0 t s r p
  in
  _ticketProb (M.toList r) p

-- Computes the probability of fields for every ticket
ticketsProb :: [Ticket] -> Rules -> Probability
ticketsProb t r =
  let
    _ticketsProb :: [Ticket] -> Probability -> Probability
    _ticketsProb [] p = p
    _ticketsProb (tk : l) p = _ticketsProb l $ ticketProb tk r p
  in
  _ticketsProb t $ M.empty

-- Removes all possibilities that are not possible enough
removeAllNotN :: Int -> Probability -> Probability
removeAllNotN i p =
  let
    f x = if x == i then Just x else Nothing
    _updateKeys :: [String] -> M.Map String Int -> M.Map String Int
    _updateKeys [] a = a
    _updateKeys (i : l) a = _updateKeys l (M.update f i a)
    _remove :: [Int] -> Probability -> Probability
    _remove [] p = p
    _remove (s : l) p =
      _remove l (M.insert s (_updateKeys (M.keys (p M.! s)) (p M.! s)) p)
  in
  _remove (M.keys p) p

-- Removes all possibilities in a probability
removeAll :: Probability -> String -> Int -> Probability
removeAll p i k =
  let
    _removeAll :: [Int] -> Probability -> Probability
    _removeAll [] p = p
    _removeAll (s : l) p
      | s == k || M.notMember i (p M.! s) = _removeAll l p
      | otherwise = _removeAll l (M.insert s (M.delete i (p M.! s)) p)
  in
  _removeAll (M.keys p) p

-- Checks if there is only one possibility remaining
onlyOne :: Probability -> Bool
onlyOne p =
  let
    _onlyOne :: [M.Map String Int] -> Bool
    _onlyOne [] = True
    _onlyOne (a : l)
      | (length $ M.toList a) > 1 = False
      | otherwise = _onlyOne l
  in
  _onlyOne $ M.elems p

-- Eliminates all possibilities
eliminatePoss :: Probability -> Probability
eliminatePoss p =
  let
    _eliminate :: [Int] -> Probability -> Probability
    _eliminate [] p = p
    _eliminate (s : l) p
      | (length $ (M.toList (p M.! s))) == 1 =
          _eliminate l $ removeAll p (fst $ head $ M.toList (p M.! s)) s
      | otherwise = _eliminate l p
    _loop :: Probability -> Probability
    _loop p
      | onlyOne p = p
      | otherwise = _loop $ _eliminate (M.keys p) p
  in
  _loop p

-- Finds the most probable rank for each field
findFields :: Probability -> Fields
findFields p =
  let
    _findFields :: Fields -> [(Int, M.Map String Int)] -> Fields
    _findFields a [] = a
    _findFields a ((i, m) : l) =
      _findFields (M.insert name i a) l
      where
        name = if m /= M.empty then fst $ head $ M.toList m else ""
  in
  _findFields M.empty $ M.toList p

-- Product of fields with departure
prodFields :: Ticket -> Fields -> Int
prodFields t f =
  let
    _prodFields :: Int -> [(String, Int)] -> Int
    _prodFields a [] = a
    _prodFields a ((s, i) : l)
      | L.isPrefixOf "departure" s = _prodFields (a * (t !! i)) l
      | otherwise = _prodFields a l
  in
  _prodFields 1 $ M.toList f

-- Part Two
partTwo :: Ticket -> [Ticket] -> Rules -> Int
partTwo tk t r = prodFields tk f
  where
    f = findFields fp
    fp = eliminatePoss prob
    prob = removeAllNotN (length gt) tp
    tp = ticketsProb gt r
    gt = removeInvalid r t

-- Parsing rules:

-- Gets the name of a rule and returns it along with the rest of the rule
getRuleName :: String -> (String, String)
getRuleName s =
  let
    _getRuleName :: String -> String -> (String, String)
    _getRuleName a [] = (reverse a, "")
    _getRuleName a (':' : s) = (reverse a, drop 1 s)
    _getRuleName a (c : s) = _getRuleName (c : a) s
  in 
  _getRuleName "" s

-- Gets the first range in a rest of rule
getRange :: String -> (Range, String)
getRange s =
  let
    _getRange :: (String, String) -> String -> (Range, String)
    _getRange (a, b) []        = ((read $ reverse a :: Int,
                                  read $ reverse b :: Int),
                                  drop 3 s)
    _getRange (a, b) (' ' : s) = ((read $ reverse a :: Int,
                                  read $ reverse b :: Int),
                                  drop 3 s)
    _getRange (a, b) ('-' : s) = _getRange (b, a) s
    _getRange (a, b) (c : s) = _getRange (a, c : b) s
  in 
  _getRange ("", "") s

-- Gets the ranges in a rest of rule
getRanges :: String -> (Range, Range)
getRanges s = (r1, r2)
  where
    (r1, s1) = getRange s
    (r2, _) = getRange s1

-- Parses a rule
parseRule :: String -> (String, (Range, Range))
parseRule s = (name, (r1, r2))
  where
    (name, s1) = getRuleName s
    (r1, r2) = getRanges s1

-- Parses rules
parseRules :: [String] -> Rules
parseRules r =
  let
    _parseRules :: Rules -> [String] -> Rules
    _parseRules rules [] = rules 
    _parseRules rules (a : l) = 
      _parseRules (M.insert name ranges rules) l
      where
        (name, ranges) = parseRule a
  in
  _parseRules (M.empty) r

-- Reads the rules file
readRules :: String -> IO Rules
readRules s = do
  input <- readFile s
  return $ parseRules $ lines input

-- Parsing tickets:

-- Splits a string by ,
splitCommas :: String -> [String]
splitCommas str = 
  words [if c == ',' then ' ' else c | c <- str]

-- Parses an input into Ticket
parseTicket :: String -> Ticket
parseTicket input = o
  where
    sc = splitCommas input
    o = [read a :: Int | a <- sc]

-- Reads the test file
readTickets :: String -> IO [Ticket]
readTickets s = do
  input <- readFile s
  return $ [parseTicket o | o <- lines input]