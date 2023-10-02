import System.IO
import Control.Exception
import Text.Read
import Data.Maybe
import Prelude hiding (Word)
import Data.Function (on)
import Data.List (minimumBy, delete)
import Control.Monad (guard)


--code for reading input from file

readInValues :: IO [Int]
readInValues = do
    putStrLn "Please enter file path"
    path <- getLine
    file <- try (readFile path) :: IO (Either SomeException String)
    case file of
        Left exception -> do
            putStrLn "File could not be opened"
            return []
        Right contents -> do
            let inVal = mapMaybe readMaybe (words contents)
            let values = checkSize inVal
            if (values == [])
            then do
                putStrLn "Invalid input argumets"
                return []
            else
                return values
   
checkSize :: [Int] -> [Int]
checkSize [] =  []
checkSize (size:list) = if (size*size == length list && allUnique list)
                    then (size:list)
                    else []
                    
allUnique :: [Int] -> Bool
allUnique [] = True
allUnique (x:xs) = notElem x xs && allUnique xs
                    
--code for checking if puzzle is solveable
    
--Looks through each number in list starting from 1
--Counts numbers higher than current number that appear earlier in the list
getInversions :: [Int] -> Int -> Int -> Int
getInversions list size currNum =
    if currNum == (size * size)
    then 0
    
    else
    length (filter (>currNum) (take (findElem list currNum) list))
    + getInversions list size (currNum + 1)

    
--Empty tile is 0 when calling findRow
--Starting Number is 1 when calling getInversions
-- If Even Size - Inversions + empty tile row must be odd to be solvable
-- If Odd Size - Inversions must be even to be solvable
isSolvable :: [Int] -> Bool
isSolvable (size:list)
  | size `mod` 2 == 0 = ((getInversions list size 1) + (findRow list 0 size)) `mod` 2 /= 0
  | otherwise         = (getInversions list size 1) `mod` 2 == 0



--Finds desired number in list of numbers
findElem :: [Int] -> Int -> Int
findElem (x:xs) num
  | x == num  = 0
  | otherwise = 1 + findElem xs num
  
--Finds the number row from the number list and row size
findRow :: [Int] -> Int -> Int -> Int
findRow list num size = floor $ fromIntegral (findElem list num) / fromIntegral size

-- Code to solve the puzzle

-- Input: Size
generateSolution :: Int -> [Int]
generateSolution size = [1..(size^2 - 1)] ++ [0]

-- Input: Board -> Size -> prevMove
getValidMoves board size prevMove = let
      empty = findEmptyTile board 0
      in
      filter (not . null) [getValidMove empty size prevMove validMove | validMove <- ["UP", "DOWN", "LEFT", "RIGHT"]]

-- Input: Empty Tile -> Size -> Previous Move -> Valid Move
getValidMove :: Int -> Int -> [Char] -> [Char] -> [Char]
getValidMove empty size prevMove validMove
      | (y < (size - 1)) && prevMove /= "DOWN" && validMove == "UP" = "UP"
      | (y > 0) && prevMove /= "UP" && validMove == "DOWN" = "DOWN"
      | (x < (size - 1)) && prevMove /= "RIGHT" && validMove == "LEFT" = "LEFT"
      | (x > 0) && prevMove /= "LEFT" && validMove == "RIGHT" = "RIGHT"
      | otherwise = []
      where
      x = empty `mod` size
      y = (empty - x) `div` size

-- Input: Board -> Count -> Size
printBoard :: [Int] -> Int -> Int -> IO()
printBoard [] _ _ = putChar '\n'
printBoard (a:b) count size
      | count == size = do
      putChar '\n'
      printBoard (a:b) 0 size

      |otherwise =
      do
      putStr(show a)
      if (a < 10)
      then putStr "  "
      else putStr " "
      printBoard b (count+1) size

-- Input: Board -> Move List -> Size
printSolution _ [] _ = return ()
printSolution board (x:xs) size = do
      putStrLn x
      let newBoard = makeMove board x size
      printBoard newBoard 0 size
      printSolution newBoard xs size

-- Input: Board -> Move -> Size
makeMove :: [Int] -> [Char] -> Int -> [Int]
makeMove board move size
      | a < b = swapTiles a b board
      | otherwise = swapTiles b a board
      where
      a = findEmptyTile board 0
      b = findNeighbouringTile a move size

-- Input: Board -> Count = 0
findEmptyTile :: [Int] -> Int -> Int
findEmptyTile (a:b) count
      | a == 0 = count
      | otherwise = findEmptyTile b (count + 1)

-- Input: Empty Tile -> Move -> Size
findNeighbouringTile :: Int -> [Char] -> Int -> Int
findNeighbouringTile emptyTile move size
      | move == "UP" = emptyTile + size
      | move == "DOWN" = emptyTile - size
      | move == "LEFT" = emptyTile + 1
      | move == "RIGHT" = emptyTile - 1

-- Input: Index 1 -> Index 2 -> Board
swapTiles :: Int -> Int -> [a] -> [a]
swapTiles i j xs = let
      elemI = xs !! i
      elemJ = xs !! j
      left = take i xs
      middle = take (j - i - 1) (drop (i + 1) xs)
      right = drop (j + 1) xs
      in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

-- Input: Board -> Size -> Move List -> Moves Remaining -> Previous Move
attemptMove :: [Int] -> Int -> [[Char]] -> Int -> [Char] -> [[Char]]
attemptMove board size moveList movesRemaining prevMove
      | board == (generateSolution size) = moveList
      | movesRemaining <= 0 = []
      | otherwise = do
            move <- (getValidMoves board size prevMove)
            let newBoard = makeMove board move size
            attemptMove newBoard size (moveList ++ [move]) (movesRemaining - 1) move

-- Input: (Size:Board) -> Max Number of Moves
solveBoard :: [Int] -> Int -> IO()
solveBoard (size:board) maxMoves = do
      putStrLn ("Attempting with maximum " ++ show maxMoves ++ " moves")
      if (board == (generateSolution size))
      then do
            putStrLn ("Solved in 0 moves")
            printBoard board 0 size
      else
            if (moveList /= [])
            then do
                  putStrLn ("Solved in "  ++ show maxMoves ++  " moves")
                  printBoard board 0 size
                  (printSolution board moveList size)
                  print (moveList)
            else
                  solveBoard (size:board) (maxMoves + 1)

            where moveList = (attemptMove board size [] maxMoves [])

--main driver
main :: IO()
main = do
    values <- readInValues
    if (values /= [])
    then
        if (isSolvable values)
        then
            solveBoard values 1
        else
            putStrLn "puzzle not solvable"
    else return ()
        
    

