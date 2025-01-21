module Board
    ( Cell(..)
    , Board
    , board
    , flipCell
    , isBoardComplete
    ) where

import System.Random
import Data.Char (intToDigit)

-- Cell data type representing each cell on the board
data Cell = Cell
    { cellValue :: Char       -- The value when the cell is flipped
    , cellFlipped :: Bool     -- Whether the cell is flipped or not
    } deriving (Eq)

-- Show instance for Cell to determine how cells are displayed
instance Show Cell where
    show cell
        | cellFlipped cell = [' ', cellValue cell, ' ']
        | otherwise = " * "

-- Type synonym for a board, which is a list of list of Cells
type Board = [[Cell]]

-- Function to create a random board given the number of rows, columns, and values
board :: Int -> Int -> Int -> StdGen -> Board
board rows cols numValues gen =
    let halfNumCells = (rows * cols) `div` 2
        values = take numValues (cycle (['0'..'9'] ++ ['A'..'Z']))
        cellValues = take halfNumCells values ++ take (halfNumCells + (rows * cols) `mod` 2) values
        shuffledCells = shuffle cellValues gen
        boardCells = map (\v -> Cell v False) shuffledCells
    in chunksOf cols boardCells

-- Function to flip a cell at a given coordinate (row and column)
flipCell :: Board -> Int -> Int -> Board
flipCell board row col =
    let (before, current:after) = splitAt row board
        (left, cell:right) = splitAt col current
        newCell = cell { cellFlipped = not (cellFlipped cell) }
        newRow = left ++ [newCell] ++ right
    in before ++ [newRow] ++ after

-- Function to check if the board is complete (all cells are flipped)
isBoardComplete :: Board -> Bool
isBoardComplete = all (all cellFlipped)

-- Helper function to shuffle a list using a given random number generator
shuffle :: [a] -> StdGen -> [a]
shuffle [] _ = []
shuffle xs gen =
    let (index, newGen) = randomR (0, length xs - 1) gen
        (before, x:after) = splitAt index xs
    in x : shuffle (before ++ after) newGen

-- Helper function to split a list into chunks of a specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
