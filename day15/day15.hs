import qualified Data.Map as M

-- type definition
type Spoken = M.Map Int Int


-- initialises spoken
initSpoken :: [Int] -> (Int, Spoken)
initSpoken nb =
  let
    _initSpoken :: [Int] -> Int -> Spoken -> (Int, Spoken)
    _initSpoken [] i s = (i, s)
    _initSpoken (a : l) i s = 
      _initSpoken l (i + 1) (M.insert a i s)
  in
  _initSpoken nb 1 M.empty

-- speak a number
speak :: Int -> Int -> Spoken -> (Int, Spoken)
speak l i spoken
  | M.notMember l spoken = (0, M.insert l (i - 1) spoken)
  | otherwise = 
    ((i - 1) - (spoken M.! l), M.insert l (i - 1) spoken)

-- finds the next number to be spoken
nextSpeak :: Int -> Int -> Spoken -> Int
nextSpeak l i spoken
  | M.notMember l spoken = 0
  | otherwise = (i - 1) - (spoken M.! l)

-- speak all numbers until n
speakN :: Int -> [Int] -> Int
speakN n nb =
  let
    _speakN :: Int -> Spoken -> Int -> Int
    _speakN l spoken i
      | i > n = l 
      | otherwise = 
        _speakN (nextSpeak l i spoken) (M.insert l (i - 1) spoken) $! i + 1
  in
  _speakN l s i
  where
    (i, s) = initSpoken nb
    l = last nb

-- part one
partOne :: [Int] -> Int
partOne = speakN 2020

-- part two (there probably is a better way but hey)
partTwo :: [Int] -> Int
partTwo = speakN 30000000

-- Splits a string by ,
splitCommas :: String -> [String]
splitCommas str = 
  words [if c == ',' then ' ' else c | c <- str]

-- Parses an input into [Int]
parseInput :: String -> [Int]
parseInput input = o
  where
    sc = splitCommas input
    o = [read a :: Int | a <- sc]

-- Reads the test file
readTest :: IO [Int]
readTest = do
  input <- readFile "inputs/testday15.txt"
  return $ parseInput input

-- Reads the input file
readInput :: IO [Int]
readInput = do
  input <- readFile "inputs/day15.txt"
  return $ parseInput input