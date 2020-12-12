-- Type Definition
type East = Int
type North = Int
type Angle = Int
type Ship = (Angle, East, North)
type Waypoint = (East, North)
type Instruction = (Char, Int)
type Instructions = [Instruction]

-- Converts a given 90 degrees multiple angle to its cos
rCos :: Angle -> Int
rCos 0 = 1
rCos 90 = 0
rCos 180 = -1
rCos 270 = 0
rCos 360 = 1

-- Converts a given 90 degrees multiple angle to its sin
rSin :: Angle -> Int
rSin 0 = 0
rSin 90 = 1
rSin 180 = 0
rSin 270 = -1
rSin 360 = 0

-- Rotates the wp by a given 90 degrees multiple angle
rotateWP :: Angle -> Waypoint -> Waypoint
rotateWP 0 wp = wp
rotateWP 90 (e, n) = (-n, e)
rotateWP 180 (e, n) = (-e, -n)
rotateWP 270 (e, n) = (n, -e)
rotateWP 360 wp = wp

-- Reads and executes an Instruction
executeInstruction :: Instruction -> Ship -> Ship
executeInstruction (i, p) (ad, e, n)
  | i == 'N' = (ad, e, n + p)
  | i == 'S' = (ad, e, n - p)
  | i == 'E' = (ad, e + p, n)
  | i == 'W' = (ad, e - p, n)
  | i == 'R' = ((ad - p) `mod` 360, e, n)
  | i == 'L' = ((ad + p) `mod` 360, e, n)
  | i == 'F' = (ad, e + p * rCos ad, n + p * rSin ad)
  | otherwise = (ad, e, n)

-- Reads and executes a list of Instruction
executeInstructions :: Instructions -> Ship -> Ship
executeInstructions [] ship = ship
executeInstructions (i : l) ship =
  executeInstructions l $ executeInstruction i ship

-- Reads and executes an Instruction with waypoint
executeNewInstruction :: Instruction -> Waypoint -> Ship -> (Waypoint, Ship)
executeNewInstruction (i, p) (we, wn) ship
  | i == 'N' = ((we, wn + p), ship)
  | i == 'S' = ((we, wn - p), ship)
  | i == 'E' = ((we + p, wn), ship)
  | i == 'W' = ((we - p, wn), ship)
  | i == 'R' = (rotateWP ((-p) `mod` 360) (we, wn), ship)
  | i == 'L' = (rotateWP (p `mod` 360) (we, wn), ship)
executeNewInstruction ('F', p) (we, wn) (ad, e, n) =
  ((we, wn), (ad, e + me, n + mn))
  where (me, mn) = (p * we, p * wn)
executeNewInstruction (i, p) wp ship = (wp, ship)

-- Reads and executes a list of Instruction with waypoint
executeNewInstructions :: Instructions -> Waypoint -> Ship -> Ship
executeNewInstructions [] _ ship = ship
executeNewInstructions (i : l) wp ship =
  executeNewInstructions l nwp nship
  where 
    (nwp, nship) = executeNewInstruction i wp ship

-- Gives the Manhattan distance of the ship
manhattan :: Ship -> Int
manhattan (_, e, n) = abs e + abs n

-- Part One
partOne :: Instructions -> Int
partOne i = manhattan $ 
            executeInstructions i (0, 0, 0)

-- Part Two
partTwo :: Instructions -> Int
partTwo i = manhattan $ 
            executeNewInstructions i (10, 1) (0, 0, 0)

-- Reads the input file and returns Instructions
readInput :: IO Instructions
readInput = do
  input <- readFile "days/inputs/day12.txt"
  return $ [(i, read t :: Int) | i : t <- lines input]

-- Reads the test file and returns Instructions
readTest :: IO Instructions
readTest = do
  input <- readFile "days/inputs/testday12.txt"
  return $ [(i, read t :: Int) | i : t <- lines input]