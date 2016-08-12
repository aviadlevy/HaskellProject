module Chess where

import Data.List.Split
import Data.List
import Data.Char
import System.Console.ANSI
import System.Exit
import Control.Monad
import Data.Maybe

import Board
import Validate

-- move piece from one to other location on board, if possible
move :: Player -> (Int, Int) -> (Int, Int) -> Board -> Either String Board
move p from to b = if isValid p source target b
                   then Right b''
                   else Left "Invalid move"
                       where piece  = fromJust $ lookup from b
                             source = (from, piece)
                             piece' = fromJust $ lookup to b
                             target = (to, piece')
                             b'     = setPiece p (from, piece) to b
                             b''    = setPiece p (to, Nothing) from b'
                             
-- from user input to board coordinate
getMoveCoords :: String -> IO ((Int, Int), (Int, Int))
getMoveCoords msg = do putStr msg
                       x <- getLine
                       if (length x) == 4 && not (isValidCoords (fmt $ map letterToNum x))
                       then return . fmt $ map letterToNum x
                       else getMoveCoords "Invalid input. The move should be in the form of: [A-H][1-8][A-H][1-8].\nplease try again: "
                           where fmt [x1, y1, x2, y2] = ((x1,y1), (x2, y2))


-- check if user input is legal
isValidCoords :: ((Int, Int), (Int, Int)) -> Bool
isValidCoords ((x1,y1), (x2, y2)) = 8 `elem` [x1, y1, x2, y2]

-- check if player is in check
checkChess :: Player -> Board -> IO ()
checkChess color b | isChess color (findKing color (0,0) b) b = if canMove color (0, 0) (0, 0) b then gameLoop color b (Just "Check!")
                                                                else gameLoop color b (Just ("CheckMate!"))
                   | otherwise = gameLoop color b Nothing

gameLoop :: Player -> Board -> Maybe String -> IO ()
gameLoop color b msg = do 
                               clearScreen
                               putStrLn $ writeBoard b
                               case msg of
                                   Just "CheckMate!" -> putStrLn ((fromJust msg) ++ "\n" ++ show (nextColor color) ++ " is the WINNER!")
                                   Just msg -> putStrLn msg
                                   Nothing -> putStrLn ""
                               when (msg == Just "CheckMate!") exitSuccess
                               coords <- getMoveCoords ((show color) ++ ": Enter your move: ")
                               case move color (fst coords) (snd coords) b of
                                   Right b'         -> checkChess (nextColor color) b'
                                   Left  msg        -> gameLoop color b (Just msg)

main = let board = readBoard initialBoard
       in gameLoop Board.White board (Just "The move should be in the form of: [A-H][1-8][A-H][1-8]")

-- conver letter to a number       
letterToNum :: Char -> Int
letterToNum 'A' = 0
letterToNum 'B' = 1
letterToNum 'C' = 2
letterToNum 'D' = 3
letterToNum 'E' = 4
letterToNum 'F' = 5
letterToNum 'G' = 6
letterToNum 'H' = 7
letterToNum '1' = 0
letterToNum '2' = 1
letterToNum '3' = 2
letterToNum '4' = 3
letterToNum '5' = 4
letterToNum '6' = 5
letterToNum '7' = 6
letterToNum '8' = 7
letterToNum  _  = 8
