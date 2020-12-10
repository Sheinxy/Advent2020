import Data.List
import Data.Map

-- Type definition
type Adapter = Int
type Adapters = [Adapter]
type JoltDiff = (Int, Int, Int)

-- Connects every adapter
connectAdapters :: Adapters -> JoltDiff
connectAdapters adapters =
  let
    _connectAdapters :: JoltDiff -> Adapter -> Adapters -> JoltDiff
    _connectAdapters (a, b, c) _ [] = (a, b, c + 1)
    _connectAdapters (a, b, c) prev (cur : l)
      | cur - prev == 1 = 
        _connectAdapters (a + 1, b    , c    ) cur l 
      | cur - prev == 2 = 
        _connectAdapters (a    , b + 1, c    ) cur l 
      | cur - prev == 3 = 
        _connectAdapters (a    , b    , c + 1) cur l 
      | otherwise = error "connectAdapters: Difference too high"
  in 
  _connectAdapters (0, 0, 0) 0 $ sort adapters

-- Part 2
countPath adapters =
  let
    _countPath sol [] = sol ! (maximum adapters) 
    _countPath sol (cur : l) =
      _countPath s l 
      where 
      s = Data.Map.insert cur v sol
      v = ( (if member (cur - 1) sol then sol ! (cur - 1) else 0) + (if member (cur - 2) sol then sol ! (cur - 2) else 0) + (if member (cur - 3) sol then sol ! (cur - 3) else 0))
  in
  _countPath (fromList [(0, 1)]) $ sort adapters


-- Reads the input file and returns [Int]
readInput :: IO [Int]
readInput = do
  input <- readFile "days/inputs/day10.txt"
  return ([read l :: Int | l <- lines input])