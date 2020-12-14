import qualified Data.Map as M

--type definition
type BinaryString = String
type Mask = BinaryString
type Address = Int
type MemInstruction = (Address, Int)
type Memory = M.Map Address Int
type Instruction = String
type Program = [Instruction]

-- Converts a number to a binary String
intToBin :: Int -> BinaryString
intToBin a =
  let
    _intToBin :: BinaryString -> Int -> BinaryString
    _intToBin acc 0 = acc
    _intToBin acc i = _intToBin ((head $ show (i `mod` 2)) : acc) (i `div` 2)
  in
  _intToBin "" a

-- Converts a binary string to a number
binToInt :: BinaryString -> Int
binToInt b =
  let
    _binToInt :: Int -> BinaryString -> Int
    _binToInt acc [] = acc
    _binToInt acc (c : l) = _binToInt (2 * acc + (read (c : "") :: Int)) l 
  in
  _binToInt 0 b

-- Applies a mask to a value
applyMask :: Mask -> BinaryString -> BinaryString
applyMask mask b =
  let
    _applyMask :: BinaryString -> Mask -> BinaryString -> BinaryString
    _applyMask acc [] [] = acc
    _applyMask _ [] _ = error "applyMask: mask too short"
    _applyMask acc ('1' : m) [] = _applyMask ('1' : acc) m []
    _applyMask acc ('1' : m) (_ : b) = _applyMask ('1' : acc) m b
    _applyMask acc ('0' : m) [] = _applyMask ('0' : acc) m []
    _applyMask acc ('0' : m) (n : b) = _applyMask (n : acc) m b
    _applyMask acc ('X' : m) [] = _applyMask ('X' : acc) m []
    _applyMask acc ('X' : m) (_ : b) = _applyMask ('X' : acc) m b
  in 
  _applyMask [] (reverse mask) (reverse b)


-- Checks if line is a memory instruction
isMem :: Instruction -> Bool
isMem s = take 4 s == "mem["

-- Parses a memory instruction
parseMem :: Instruction -> MemInstruction
parseMem s =
  let
    _getAddress :: String -> String -> (Int, String)
    _getAddress acc [] = error "parseMem: invalid format"
    _getAddress acc (']' : l) = (read $ reverse acc :: Int, l)
    _getAddress acc (c : l) = _getAddress (c : acc) l

    _getInstruction m = (addrs, v)
      where
        (addrs, la) = _getAddress "" m
        v = read $ drop 3 la :: Int
  in
  _getInstruction $ drop 4 s

-- Parses a mask instruction
parseMask :: Instruction -> Mask
parseMask = drop 7

-- Finds all the memory addresses to write in
findAddresses :: BinaryString -> [Address]
findAddresses bstring =
  let
    _findAddresses :: [BinaryString] -> BinaryString -> [Address]
    _findAddresses acc [] = map binToInt acc
    _findAddresses acc ('X' : s) = _findAddresses c s
      where
        a = map (\x -> '0' : x) acc
        b = map (\x -> '1' : x) acc
        c = a ++ b
    _findAddresses acc (c : s) = _findAddresses (map (\x -> c : x) acc) s
  in
  _findAddresses [""] $ reverse bstring

-- Inserts a memory instruction inside a memory
insertMem :: Mask -> Instruction -> Memory -> Memory
insertMem m i mem = 
  let
    _insertMem :: [Address] -> Memory -> Memory
    _insertMem [] mem = mem
    _insertMem (ad : l) mem = _insertMem l $ M.insert ad v mem
  in
  _insertMem addresses mem
  where
    (a, v) = parseMem i
    addresses = findAddresses $ applyMask m $ intToBin a

-- Executes a program and returns the memory
executeProgram :: Program -> Memory
executeProgram p =
  let
    _executeProgram :: Memory -> Mask -> Program -> Memory
    _executeProgram mem _ [] = mem
    _executeProgram mem m (i : p)
      | isMem(i) = _executeProgram (insertMem m i mem) m p
      | otherwise = _executeProgram mem (parseMask i) p
  in
  _executeProgram M.empty "000000000000000000000000000000000000" p

-- Sums the value of each address in the memory
sumMemory :: Memory -> Int
sumMemory m = sum $ M.elems m 

-- Part Two
partTwo :: Program -> Int
partTwo input = sumMemory $ executeProgram input

-- Reads the test file
readTest :: IO Program
readTest = do
  input <- readFile "inputs/test2day14.txt"
  return $ lines input

-- Reads the input file
readInput :: IO Program
readInput = do
  input <- readFile "inputs/day14.txt"
  return $ lines input