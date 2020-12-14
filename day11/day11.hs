import Data.Map

-- Type Definition
type Position = (Int, Int)
type Board = Map Int (Map Int Char)
type BoardData = (Int, Int, Board)

-- Counts occupied neighbours of a cell
countNeighbours :: Position -> BoardData -> Int
countNeighbours (x, y) (mx, my, board) =
  let
    _loop (dx, dy) acc
      | dx > 1 = acc
      | dy > 1 = _loop (dx + 1, -1) acc
      | dx == 0 && dy == 0 = _loop (dx, dy + 1) acc
      | y + dy < 0 = _loop (dx, dy + 1) acc
      | y + dy >= my = _loop (dx + 1, -1) acc
      | x + dx < 0 = _loop (dx + 1, -1) acc
      | x + dx >= mx = acc
      | board ! (x + dx) ! (y + dy) == '#' = _loop (dx, dy + 1) (acc + 1)
      | otherwise = _loop (dx, dy + 1) acc
  in
  _loop (-1, -1) 0

-- Counts the number of occupied seats seen by a cell
countSeen :: Position -> BoardData -> Int
countSeen (x, y) (mx, my, board) = 
  let
    _loop :: Position -> (Int, Int) -> Int
    _loop (x, y) (dx, dy)
      | x < 0 || x >= mx || y < 0 || y >= my = 0
      | board ! x ! y == '#' = 1
      | board ! x ! y == 'L' = 0
      | otherwise = _loop (x + dx, y + dy) (dx, dy)
  in
  _loop (x - 1, y - 1) (-1, -1) +_loop (x, y - 1) (0, -1) + _loop (x + 1, y - 1) (1, -1) +
  _loop (x - 1, y) (-1, 0)      +             0           + _loop (x + 1, y) (1, 0) +
  _loop (x - 1, y + 1) (-1, 1)  +_loop (x, y + 1) (0, 1) + _loop (x + 1, y + 1) (1, 1)
  
-- Applies the rules to the board at a specific position
applyRules :: Int -> (Position -> BoardData -> Int) -> Position -> Board -> BoardData -> BoardData
applyRules e f (x, y) o_b (mx, my, board)
  | c == 'L' && o == 0 = (mx, my, insert x (insert y '#' (board ! x)) board)
  | c == '#' && o >= e  = (mx, my, insert x (insert y 'L' (board ! x)) board)
  | otherwise = (mx, my, board)
  where 
    o = f (x, y) (mx, my, o_b)
    c = (o_b ! x) ! y

-- Updates the board for a round
updateBoard :: Int -> (Position -> BoardData -> Int) -> BoardData -> BoardData
updateBoard e f (mx, my, board) =
  let
    _loop :: Position -> BoardData -> BoardData
    _loop (x, y) (mx, my, b)
      | y >= my = (mx, my, b)
      | x >= mx = _loop (0, y + 1) (mx, my, b)
      | otherwise = 
        _loop (x + 1, y) $ 
        applyRules e f (x, y) board (mx, my, b)
  in
  _loop (0, 0) (mx, my, board)

-- Counts the number of occupied seats in a board
occupiedSeats :: BoardData -> Int
occupiedSeats (mx, my, board) = 
  let
    _loop :: Position -> Board -> Int -> Int
    _loop (x, y) board acc
      | y >= my = acc
      | x >= mx = _loop (0, y + 1) board acc
      | board ! x ! y == '#' = _loop (x + 1, y) board (acc + 1)
      | otherwise = _loop (x + 1, y) board acc
  in
  _loop (0, 0) board 0 

-- Updates the board until the number of occupied seats stabilises
play :: Int -> (Position -> BoardData -> Int) -> BoardData -> Int
play e f board =
  let
    _play :: Int -> Int -> BoardData -> Int
    _play m l board 
      | m == l && occupied == l = occupied
      | otherwise = 
        _play l occupied $ 
        updateBoard e f board
      where occupied = occupiedSeats board
  in
  _play (-1) (-1) board
play1 = play 4 countNeighbours
play2 = play 5 countSeen

-- Converts a list of String to a BoardData
linesToData :: [String] -> BoardData
linesToData l = 
  (length l, length (head l), fromList $ 
                              Prelude.map (\(x, y) -> (x, fromList $ 
                              zip [0..] y)) $ 
                              zip [0..] l)

-- Reads the input file and returns Board
readInput :: IO [String]
readInput = do
  input <- readFile "days/inputs/day11.txt"
  return $ lines input

-- Reads the test file and returns Board
readTest :: IO [String]
readTest = do
  input <- readFile "days/inputs/testday11.txt"
  return $ lines input