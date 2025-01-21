module Main where

import Board
import Control.Monad (when)
import qualified Data.Ini.Config as I
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game (Game(..), play)
import Paths_Final (getDataFileName)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.Random (newStdGen)
import qualified Text.CSV as C

-- Function to display scores
showScores :: C.CSV -> IO ()
showScores [] = return ()
showScores (("":_) : rs) = showScores rs
showScores ((t:s:_) : rs) = do
    putStrLn $ "|" ++ t ++ replicate (20 - length t) ' ' ++ "|" ++ s ++ replicate (7 - length s) ' ' ++ "|"
    showScores rs
showScores _ = die "\nError parsing score data"

-- Function to read scores from a file
readScores :: IO ()
readScores = do
    p <- getDataFileName "config/scores.csv"
    e <- doesFileExist p
    if e
        then do
            let line = "+" ++ replicate 19 '-' ++ "+" ++ replicate 6 '-' ++ "+"
            putStrLn line
            putStrLn "|              date|   score|"
            putStrLn line
            csv <- C.parseCSVFromFile p
            case csv of
                Left err -> die $ "Error reading scores: " ++ show err
                Right scores -> do
                    showScores scores
                    putStrLn line
        else putStrLn "There are currently no scores to display."

-- Function to read configuration from a file
readCfg :: T.Text -> Either String (Maybe Int, Maybe Int, Maybe Int, Maybe Double)
readCfg cfg = I.parseIniFile (T.concat [T.pack "[MAIN]\n", cfg]) $ do
    I.section (T.pack "MAIN") $ do
        w <- I.fieldMbOf (T.pack "width") I.number
        h <- I.fieldMbOf (T.pack "height") I.number
        v <- I.fieldMbOf (T.pack "values") I.number
        t <- I.fieldMbOf (T.pack "wait-time") I.number
        return (w, h, v, t)

getCfg :: IO (Maybe Int, Maybe Int, Maybe Int, Maybe Double)
getCfg = do
    p <- getDataFileName "config/memory-game.ini"
    e <- doesFileExist p
    if e
        then do
            ini <- T.readFile p
            case readCfg ini of
                Left err -> die $ "Error reading config: " ++ err  -- err is a String, so just use it directly
                Right cfg -> return cfg
        else return (Nothing, Nothing, Nothing, Nothing)


-- Function to start the game
start :: IO ()
start = do
    putStrLn "1. New Game"
    putStrLn "2. Scores"
    putStrLn "3. Guide"
    putStrLn "4. Quit"
    putStrLn "Please pick your choice: "
    choice <- getLine
    case choice of
        "1" -> promptNewGame
        "2" -> readScores
        "3" -> showGuide
        "4" -> putStrLn "Exiting the game. Thank you for playing!"
        _   -> putStrLn "Invalid option, please try again." >> start

promptNewGame :: IO ()
promptNewGame = do
    putStrLn "Enter the number of rows: "
    rowsStr <- getLine
    putStrLn "Enter the number of columns: "
    colsStr <- getLine
    let rows = read rowsStr :: Int
        cols = read colsStr :: Int
    newGame rows cols

newGame :: Int -> Int -> IO ()
newGame rows cols = do
    gen <- newStdGen
    let bs = board rows cols (rows * cols) gen
    play $ Game 0 2 1 bs

showGuide :: IO ()
showGuide = do
    putStrLn "Welcome to the Memory Game!"
    putStrLn "The objective of the game is to find all matching pairs of tiles."
    putStrLn "Enter the coordinates (e.g., A1, B2) of the tile you wish to flip."
    putStrLn "If two flipped tiles match, they remain open. Otherwise, they close."
    putStrLn "The game continues until all pairs are matched."
    putStrLn "Enter 'q' or 'Q' to quit the game at any time."
    start

main :: IO ()
main = do
    putStrLn "<Memory Game - Main Menu>"
    start
