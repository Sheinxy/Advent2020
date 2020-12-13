import Data.List

-- Type Definition
type ID = Int
type Time = Int
type Buses = [ID]
type Note = (Time, Buses)
type Depature = (Time, ID)
type BusOffset = (ID, Time)
type Constrains = [BusOffset]

-- Computes the earliest depature time available
earliest :: Time -> ID -> Time
earliest timestamp bus
  | timestamp `mod` bus == 0 = timestamp
  | otherwise = (timestamp `div` bus) * bus + bus

-- Finds the earliest bus possible
findEarliest :: Note -> Depature
findEarliest (t, (b : l)) =
  let
    _findEarliest :: Buses -> Depature -> Depature
    _findEarliest [] d = d
    _findEarliest (b : l) (earliestT, earliestB)
      | e < earliestT = _findEarliest l (e, b)
      | otherwise = _findEarliest l (earliestT, earliestB)
      where 
        e = earliest t b
  in
  _findEarliest l $ (earliest t b, b)

-- Part One
partOne :: Note -> Int
partOne (t, buses) = (e - t) * b
  where
    (e, b) = findEarliest (t, buses)

-- Splits a string by ,
splitCommas :: String -> [String]
splitCommas str = 
  words [if c == ',' then ' ' else c | c <- str]

-- Parses an input to a Notes
parseInputOne :: String -> Note
parseInputOne input = (t, bs)
  where
    t = read $ head $ lines input :: Int
    lbs = splitCommas $ head $ tail $ lines input
    bs = [read b :: Int | b <- lbs, b /= "x"] 

-- Reads the input file and returns Notes
readInputOne :: String -> IO Note
readInputOne inputFile = do
  input <- readFile inputFile
  return $ parseInputOne input


-- Finds the earliest timestamp respecting constrains
findConstrains :: Constrains -> Time
findConstrains [] = 0
findConstrains l =
  let
    _findConstrains :: Constrains -> Time -> Time -> Time
    _findConstrains [] t _ = t
    _findConstrains ((b, o) : l) t r
      | t `mod` b == o = _findConstrains l t (b * r)
      | otherwise = _findConstrains ((b, o) : l) (t + r) r
  in
  _findConstrains c o a
  where
    rl = reverse $ sort l
    c = tail rl
    (a, o) = head rl

-- Parses an input to a Constrains
parseInputTwo :: String -> Constrains
parseInputTwo input = bs
  where
    lbs = zip [0 ..] $
          splitCommas $ head $ tail $ lines input
    bs = [(read b :: Int, (-i) `mod` (read b :: Int)) | (i, b) <- lbs, b /= "x"]

-- Reads the input file and returns Constrains
readInputTwo :: String -> IO Constrains
readInputTwo inputFile = do
  input <- readFile inputFile
  return $ parseInputTwo input