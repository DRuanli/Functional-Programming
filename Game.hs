module Game
    ( Game(..)
    , play
    ) where

import Board
import System.Random
import Data.Char (isLetter, toUpper, chr, ord)
import Control.Monad (forM_)

data Game = Game
    { score :: Int
    , waitTime :: Double
    , turnCount :: Int
    , gameBoard :: Board
    }

play :: Game -> IO ()
play game = do
    putStrLn "\nWelcome to the Memory Game!"
    putStrLn "You can flip cells by entering their coordinates (e.g., A1, B2)."
    putStrLn "Enter 'q' or 'Q' to quit the game."
    gameLoop game

gameLoop :: Game -> IO ()
gameLoop game@(Game currentScore _ _ board) = do
    displayBoardWithLabels board
    putStrLn "Enter the coordinates of the first cell to flip (e.g., A1): "
    input1 <- getLine
    if input1 `elem` ["q", "Q"]
        then putStrLn "Thanks for playing!"
        else do
            let (row1, col1) = parseCoordinates input1
            if isValidCoordinate row1 col1 board
                then do
                    let updatedBoard1 = flipCell board row1 col1
                    displayBoardWithLabels updatedBoard1
                    putStrLn "Press Enter to continue..."
                    _ <- getLine
                    putStrLn "Enter the coordinates of the second cell to flip (e.g., A2): "
                    input2 <- getLine
                    if input2 `elem` ["q", "Q"]
                        then putStrLn "Thanks for playing!"
                        else do
                            let (row2, col2) = parseCoordinates input2
                            if isValidCoordinate row2 col2 updatedBoard1
                                then do
                                    let updatedBoard2 = flipCell updatedBoard1 row2 col2
                                    if cellValuesMatch updatedBoard2 row1 col1 row2 col2
                                        then do
                                            putStrLn "Match found! You earned 2 points."
                                            let newScore = currentScore + 2
                                            let newGame = game { score = newScore, gameBoard = updatedBoard2, turnCount = turnCount game + 1 }
                                            gameLoop newGame
                                        else do
                                            putStrLn "No match found."
                                            putStrLn "Press Enter to continue..."
                                            _ <- getLine
                                            let newGame = game { gameBoard = resetFlippedCells updatedBoard2, turnCount = turnCount game + 1 }
                                            gameLoop newGame
                                else do
                                    putStrLn "Invalid coordinates. Please try again."
                                    gameLoop game
                else do
                    putStrLn "Invalid coordinates. Please try again."
                    gameLoop game

displayBoardWithLabels :: Board -> IO ()
displayBoardWithLabels board = do
    let numRows = length board
    let numCols = length (head board)
    putStrLn $ " " ++ replicate (4 * numCols - 1) '*'
    putStr "  "
    putStrLn $ concatMap (\c -> " " ++ [c] ++ " |") ['A'..(chr $ ord 'A' + numCols - 1)]
    forM_ (zip [0..] board) $ \(r, row) -> do
        putStr [toEnum (fromEnum 'A' + r)]
        forM_ row $ \cell -> putStr $ " " ++ show cell ++ " |"
        putStrLn ""
    putStrLn $ " " ++ replicate (4 * numCols - 1) '*'

parseCoordinates :: String -> (Int, Int)
parseCoordinates input = 
    case words (map toUpper input) of
        [row, col] -> 
            let rowIdx = ord (head row) - ord 'A'
                colIdx = ord (head col) - ord 'A'
            in (rowIdx, colIdx)
        _ -> error "Invalid input format"


isValidCoordinate :: Int -> Int -> Board -> Bool
isValidCoordinate row col board =
    row >= 0 && row < length board && col >= 0 && col < length (head board)

cellValuesMatch :: Board -> Int -> Int -> Int -> Int -> Bool
cellValuesMatch board row1 col1 row2 col2 =
    let cell1 = (board !! row1) !! col1
        cell2 = (board !! row2) !! col2
    in cellValue cell1 == cellValue cell2 && cellFlipped cell1 && cellFlipped cell2

resetFlippedCells :: Board -> Board
resetFlippedCells board = map (map resetCell) board
  where resetCell cell = if cellFlipped cell then cell { cellFlipped = False } else cell
