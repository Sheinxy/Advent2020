-- Type definition
type Instruction = (String, Int)
type Program = [Instruction]


-- Executes a given program, exits when an infinite loop
executeProgram :: Program -> (Int, Int)
executeProgram [] = (0, 0)
executeProgram prog =
  let
    _executeProgram :: Int -> Program -> Int -> [Int] -> Program -> (Int, Int)
    _executeProgram acc _ c v [] = (acc, 0)
    _executeProgram acc prog c v ((i, p) : l)
      | c `elem` v = (acc, -1)
      | i == "acc" = 
        _executeProgram (acc + p) prog (c + 1) (c : v) l
      | i == "jmp"  && p > 0 = 
        _executeProgram acc prog (c + p) (c : v) $ 
        drop (p - 1) l
      | i == "jmp" && p < 0=
        _executeProgram acc prog (c + p) (c : v) $ 
        drop (c + p) prog
      | otherwise = 
        _executeProgram acc prog (c + 1) (c : v) l
  in
  _executeProgram 0 prog 0 [] prog 

-- Executes a given program, tries to fix any infinite loop
executeFix :: Program -> (Int, Int)
executeFix [] = (0, 0)
executeFix prog =
  let
    _executeFix :: Program -> Int -> Program -> (Int, Int)
    _executeFix prog c [] = executeProgram prog
    _executeFix prog c (("jmp", p) : l) =
      let 
        (a, r) = 
          executeProgram ((take c prog) ++ (("nop", p):l))
      in
      if r == -1 then _executeFix prog (c + 1) l
      else (a, r)
    _executeFix prog c (("nop", p) : l) =
      let 
        (a, r) = 
          executeProgram ((take c prog) ++ (("jmp", p):l))
      in
      if r == -1 then _executeFix prog (c + 1) l
      else (a, r)
    _executeFix prog c (_ : l) = 
      _executeFix prog (c + 1) l
  in
  _executeFix prog 0 prog 

-- Parses a string "name parameter" into an Instruction
parseInstruction :: String -> Instruction
parseInstruction i = 
  let
    (n : ps : _) = words i
  in
    if ps !! 0 == '+' then
      (n, read (drop 1 ps) :: Int)
    else 
      (n, read ps :: Int)

-- Reads the input file and returns a Program
readInput :: IO Program
readInput = do
  input <- readFile "days/inputs/day8.txt"
  return ([parseInstruction l | l <- lines input])