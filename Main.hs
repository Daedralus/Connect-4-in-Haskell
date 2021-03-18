--Connect 4 by Berk Demir and Matthew Smith
import Data.List
import Data.Char

----------------------------------------------------------------------

--For flexibility, we define constants for the row and column size of the
--board, length of a winning sequence, and search depth for the game tree:

numRows :: Int
numRows = 6
numCols :: Int
numCols = 7
win :: Int
win = 4
depth :: Int
depth = 5

--The board itself is represented as a list of rows, where each row is
--a list of player values, subject to the above row and column sizes:

type Board = [Row]

type Row = [Player]

--In turn, a player value is either a nought, a blank, or a cross, with
--a blank representing a position on the board that is not yet occupied:

data Player = O | B | X
              deriving (Ord, Eq, Show)
playerChar :: Player
playerChar = X
aiChar :: Player
aiChar = O

--The following code displays a board on the screen:

showBoard :: Board -> IO ()
showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
              where
                 showRow = map showPlayer
                 line    = replicate numCols '*'
                 nums    = take numCols ['0'..]
showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '-'
showPlayer X = 'X'

----------------------------------------------------------------------

--From here we should generate some primitive operations on boards, specificially
--an empty board, different views of a board and how to tell if someone has won.

emptyBoard :: Board
emptyBoard = replicate numRows (replicate numCols B)

rows :: Board -> Board
rows = id

cols :: Board -> Board
cols = transpose

--Diagonals are harder to compute, so we decided we would do it by indexing every
--item across diagonal lines. We define those diagonal lines in `upCoords` and `downCoords'.

type Coord = (Int, Int)

getPointsInRange :: [Int] -> [Int] -> (Int -> Int) -> [Coord]
getPointsInRange xs ys f = filter (\(_, y) -> elem y ys) $ map (\x -> (x, f x)) xs

getPointsInBoard :: (Int -> Int) -> [Coord]
getPointsInBoard = getPointsInRange [0..numCols-1] [0..numRows-1]
downCoords :: [[Coord]]
downCoords = map getPointsInBoard downFuncs where
  downFuncs = (map (\c x-> x + c) [(-numCols+1)..numRows-1])
upCoords :: [[Coord]]
upCoords = map getPointsInBoard upFuncs where
  upFuncs = (map (\c x-> -x + c) [0..(numCols+numRows-2)])

--Now we have these, we can then index across these lines

indexCoord :: [[a]] -> Coord -> a
indexCoord xss (x, y) = (xss !! y) !! x
indexCoords :: [[a]] -> [Coord] -> [a]
indexCoords xss cs = map (indexCoord xss) cs
downDiagonal :: [[a]] -> [[a]]
downDiagonal b = map (indexCoords b) downCoords
upDiagonal :: [[a]] -> [[a]]
upDiagonal b = map (indexCoords b) upCoords

--Now to declare the winning conditions, specificially that there are `win`
--items for a player in a row, column or diagonal.

inRowEq :: Int -> [Player] -> Player
inRowEq n xs = if null $ groups then
         B
     else
         head $ head groups
  where
      groups = filter (\x -> length x >= n && head x /= B) $ group xs
winner :: Board -> Player
winner b = case checkDirection allDirections of
      [] -> B
      x -> head x
  where
      checkDirection direction = filter (/= B) $ map (inRowEq win) direction
      allDirections = concatMap (\f -> f b) [rows, cols, upDiagonal, downDiagonal]

--We can then make items that allow for us to manipulate the board, specificially working
--out at what positions we can put tokens, and moves to actually play those tokens. We
--decided to not check that you can put a token in a column before doing it, since it is
--run so often efficiency is key. It will generate a runtime error on a y or notfull column

availableCols :: Board -> [Int]
availableCols b = map snd $ filter (\(x,_) -> head x == B ) $ zip (cols b) [0..]
setAt :: [a] -> Int -> a -> [a]
setAt xs index x = take index xs ++ [x] ++ drop (index + 1) xs
playToken :: Board -> Player -> Int -> Board
playToken b p index = cols $ setAt boardCols index newCol
   where
       newCol = morphCol (boardCols !! index)
       morphCol col = (tail $ takeWhile (==B) col) ++ [p] ++ (dropWhile (==B) col)
       boardCols = cols b
availableMoves :: Board -> Player -> [Board]
availableMoves b p = map (playToken b p) $ availableCols b

--Now we get onto the actual intelligence. One of the interesting things about minmax
--is that it swaps players on each iteration so that it assumes each player makes the
--best choices for itself. Rather than pattern matching and writing basically the same
--code for each player with two changes, we decided to put those two changing factors
--into their own functions, and write one generic minmax recursive case

opposite :: Player -> Player
opposite X = O
opposite O = X
playerFunc :: Ord a => Player -> ([a] -> a)
playerFunc X = maximum
playerFunc O = minimum

--In order to make minmax not take too long, we wanted to ensure that it just defaulted to an
--unknown at a certain move depth. We did that by passing in depth as another parameter, and if
--it reaches zero, returning blank. We chose to use Blank rather than the maybe monad as it meant
--it could be sorted using the built in minimum / maximum functions still - the player will choose
--its token over the blank over the other player's. Without this, we would have had to write a custom
--minimum function.

minmax :: Int -> Player -> Board -> Player
minmax 0 _ b = winner b
minmax d p b
     | currentWinner /= B = currentWinner
     | otherwise = (playerFunc p) $ map (minmax (d-1) (opposite p)) $ (availableMoves b p)
  where
      currentWinner = winner b

--This AI function takes in all the available moves that it can make, evaluates their likely end using
--the minmax algorithm, and picks the best move, returning that board.

data Option = Opt Board Player

instance Eq Option where
  (==) (Opt _ p) (Opt _ p') = p == p'

instance Ord Option where
  compare (Opt _ p) (Opt _ p') = compare p p'

--An important node is that this AI funciton is one of the depth traversals of the minmax algorithms,#
--meaning that, we must subtract one from the depth before using our minmax function.

ai :: Board -> Board
ai b = case (playerFunc aiChar) $ map minMaxFunc moves of
  (Opt b _) -> b
  where
      minMaxFunc b =  Opt b $ minmax (depth -1) (opposite aiChar) b
      moves = (availableMoves b aiChar)

--The final thing to do is the IO. We decided to make ours safe for user input,
--validating everything and if a bad result was given (including player trying
--a full column) then the reading function is recursively called. Same with the
--gameRound, it recursively calls itself until an end is reached.

getNumber :: String -> IO Int
getNumber prompt = do
  putStrLn prompt
  s <- getLine
  if all isDigit s then
      return ((read s)::Int)
  else
      getNumber prompt
getBoardOpt :: Board -> IO Board
getBoardOpt b = do
  n <- getNumber "Enter the column you would like to make your move on..."
  if elem n (availableCols b) then do
      return (playToken b playerChar n) 
  else do
      putStrLn "Invalid column, please try again..."
      b' <- getBoardOpt b
      return b'
gameRound :: Board -> IO ()
gameRound b = do
  showBoard b
  b' <- getBoardOpt b
  if winner b' == playerChar then do
      showBoard b'
      putStrLn "Congratz, you win!"
  else do
      showBoard b'
      putStrLn "Computer player is thinking..."
      let b'' = ai b'
      if winner b'' == aiChar then do 
          showBoard b''
          putStrLn "Oops, computer rekt u..."
      else do
          gameRound b''
main :: IO ()
main = do
  putStrLn ("Welcome to Connect " ++ show win)
  putStrLn ("You are " ++ show playerChar)
  gameRound emptyBoard
