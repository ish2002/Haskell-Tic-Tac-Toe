module TicTacToe where

import Data.Char ()
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe _ _
  = Nothing
-- WHERE DO WE USE THIS

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (_ : cs)
  = p : cs
replace _ _ []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

-- r contains the rows of the board
-- c contains the columns of the board
-- d contains the diagonals of the board
-- nubs applied nub to each row, column and diagonal
-- result determines whether any row, column, or diagonal has identical values ( all X or all O) 
gameOver :: Board -> Bool
gameOver board
  = or (concat (map result [r,c,d]))
  where
    r = rows board
    c = cols board
    d = diags board
    nubs :: [[Cell]] -> [[Cell]]
    nubs x = map nub x
    result :: [[Cell]] -> [Bool]
    result y = map isTrue (nubs y)
    isTrue :: [Cell] -> Bool 
    isTrue [Taken _] = True 
    isTrue _         = False

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
-- rdMaybe checks whether the input string is in the right format and contains integers
-- position does patter matching do return Just tuple else Nothing
parsePosition :: String -> Maybe Position
parsePosition "" = Nothing
parsePosition s
  = position (rdMaybe (words s))
  where 
    rdMaybe :: [String] -> (Maybe Int , Maybe Int)
    rdMaybe [x,y]
      = ((readMaybe x :: Maybe Int), (readMaybe y :: Maybe Int))
    rdMaybe _ 
      = (Nothing , Nothing) 
    position :: (Maybe Int , Maybe Int) -> Maybe (Int,Int)
    position (Just x, Just y) 
      = Just (x,y)
    position _ 
      = Nothing 

-- returns the Board only when the cell at the entered position is Empty.
-- pos stores the index of the position in the board
-- returns Nothing in all other cases. 
tryMove :: Player -> Position -> Board -> Maybe Board
tryMove xOro (i,j) board 
  | inBound i && inBound j && isEmpty (cells !! pos) = Just ((replace pos (Taken xOro) cells),n)
  | otherwise            = Nothing
  where
    inBound :: Int -> Bool
    inBound x = x >= 0 && x < n
    isEmpty :: Cell -> Bool
    isEmpty Empty = True
    isEmpty _     = False
    (cells, n) = board
    pos = i * n + j

-------------------------------------------------------------------
-- I/O Functions

-- xOrNots function repeated calls val to print the values Taken O/X as 'O'/'X' and Empty as '-'
-- intersperse has been used to add a space in between the elements of each row
-- output contains the first row(using head function) with '\n' character added to the beginning of each consequent row(using map and tail).
prettyPrint :: Board -> IO ()
prettyPrint = putStrLn . prettyPrint'

prettyPrint' :: Board -> String
prettyPrint' board
  = concat output
  where
    output = (head listOfLines) : (map ((:) '\n') (tail listOfLines))
    listOfLines = map (intersperse ' ') (xOrNots r) 
    r = rows board
    xOrNots :: [[Cell]] -> [[Char]]
    xOrNots [] 
      = []
    xOrNots (x:xs)
      = (map val x) : xOrNots xs
    val :: Cell -> Char
    val (Taken X) = 'X'
    val (Taken O) = 'O'
    val Empty     = '-'
-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
-- pos stores the position string
takeTurn :: Board -> Player -> IO Board
takeTurn board xOro
  = do
    putStrLn ("Enter the position of your next move")
    pos <- getLine
    case parsePosition pos of
      Just _ -> case tryMove xOro (fromJust (parsePosition pos)) board of
             Nothing -> do
               putStrLn ("Position occupied/Invalid Index , please enter another position")
               takeTurn board xOro
             Just x -> do
               return (x)
      Nothing  -> do
        putStrLn ("Invalid Entry, please enter a valid position")
        takeTurn board xOro
      


-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
-- player function returns String "X" for Player X and String "O" for Player O
-- checkWinner function determines if X won or O won and displays appropriate message
-- allTaken function is used to determine whether it's a draw
playGame :: Board -> Player -> IO ()
playGame board xOro 
  = do
    putStrLn ("Your turn player " ++ show(xOro) ++ " ! The current board is:")
    prettyPrint board
    board <- takeTurn board xOro
    if gameOver board then
      do
        prettyPrint board
        if checkWinner board xOro then
          putStrLn ("Congratulations player " ++ show(xOro) ++ " you've won :)")
        else
            putStrLn ("I'm sorry you lose, Game Over :(")
    else
      if allTaken board then
        putStrLn ("Game Over, It's a Draw!")
      else
        do
          case xOro of
              X -> playGame board O
              O -> playGame board X

{-    case gameOver board of
      True -> do
        prettyPrint board
        case checkWinner board xOro of
          True -> putStrLn ("Congratulations player " ++ show(xOro) ++ " you've won :)")
          False -> putStrLn ("I'm sorry you lose, Game Over :(")
      False -> do
        case allTaken board of
          True -> putStrLn ("Game Over, It's a Draw!")
          False -> do
            case xOro of
              X -> playGame board O
              O -> playGame board X
-- version 2
-} 
-- HOW TO CHECK THIS WITHIN CASE .. OF
allTaken :: Board -> Bool
allTaken ((Taken _:cs), n) 
  = True && allTaken (cs,n)
allTaken ([], _)
  = True
allTaken _
  = False

checkWinner :: Board -> Player -> Bool
checkWinner board xOro
  = r || c || d
  where
    r = isSingleton (map nub (rows board))
    c = isSingleton (map nub (cols board))
    d = isSingleton (map nub (diags board))
    isSingleton :: [[Cell]] -> Bool
    isSingleton ([Taken x]:_) 
      = x == xOro
    isSingleton (_:xs) 
      = False || isSingleton xs
    isSingleton []
      = False

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
-- naturalNumber function is used to determine whether the size N is valid
-- createBoard has been used to initially create a board of '-' s
main :: IO ()
main 
  = do
    putStrLn ("WELCOME TO TIC-TAC-TOE ON A N x N board :D")
    putStrLn ("Enter the board size (N) : ")
    n <- getLine
    case readMaybe n :: Maybe Int of
       Nothing -> do
         putStrLn ("Invalid input, try again")
         main
       Just x -> do
         if x <= 0 then
           do
             putStrLn ("Invalid input, try again")
             main
         else
           do
             let board = (createBoard x) 
             playGame board X

createBoard :: Int -> Board
createBoard i
  = (listOfcells, i)
  where
    listOfcells = cells (i^2)
    cells :: Int -> [Cell]
    cells x
      | x == 0    = []
      | otherwise = Empty : cells (x-1)

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

