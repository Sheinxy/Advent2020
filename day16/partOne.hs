import qualified Data.Map as M

-- Type definition
type Range = (Int, Int)
type Rules = M.Map String (Range, Range)
type Ticket = [Int]

-- Find the invalid fields of a ticket
findInvalid :: Rules -> Ticket -> [Int]
findInvalid r t =
  let
    _existRange :: Int -> [(Range, Range)] -> Bool
    _existRange _ [] = False
    _existRange i (((r1, r2), (r3, r4)) :l)
      | r1 <= i && i <= r2 = True
      | r3 <= i && i <= r4 = True
      | otherwise = _existRange i l
    _findInvalid :: [Int] -> Ticket -> [(Range, Range)] -> [Int]
    _findInvalid a [] _ = a
    _findInvalid a (tk : l) rs
      | _existRange tk rs = _findInvalid a l rs
      | otherwise = _findInvalid (tk : a) l rs
  in
  _findInvalid [] t $ M.elems r

-- Sums all the invalid fields of a list of tickets
sumInvalid :: Rules -> [Ticket] -> Int
sumInvalid r t =
    let
      _sumInvalid :: Int -> [Ticket] -> Int
      _sumInvalid a [] = a
      _sumInvalid a (ticket : l) =
        _sumInvalid (a + sum (findInvalid r ticket)) l
    in
    _sumInvalid 0 t

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