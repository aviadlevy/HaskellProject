module Main where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import System.Console.ANSI

import Chess

finished   :: Board -> Bool
finished b = False 

move             :: Player -> (Int, Int) -> (Int, Int) -> Board -> Either String Board
move p from to b = if isValid source to b
                   then Right b''
                   else Left "Invalid move"
                       where piece  = fromJust $ lookup from b
                             source = (from, piece)
                             b'     = setPiece p (from, piece) to b
                             b''    = setPiece p (to, Nothing) from b'
                             
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

getMoveCoords :: String -> IO ((Int, Int), (Int, Int))
getMoveCoords msg = do putStr msg
                       x <- getLine
                       if (length x) == 4
                       then return . fmt $ map letterToNum x
                       else getMoveCoords "Invalid input. please try again: "
                           where fmt [x1, y1, x2, y2] = ((x1,y1), (x2, y2))


isValidCoords :: ((Int, Int), (Int, Int)) -> Bool
isValidCoords ((x1,y1), (x2, y2)) = 8 `elem` [x1, y1, x2, y2]
                        

gameLoop color b msg = do 
                        clearScreen
                        case msg of
                            Just msg -> putStrLn msg
                            Nothing -> putStrLn ""
                        putStrLn $ writeBoard b
                        coords <- getMoveCoords ((show color) ++ ": Enter your move in the form: 'xyxy': ")
                        if isValidCoords coords
                        then getMoveCoords "Invalid input. please try again: "
                        else case move color (fst coords) (snd coords) b of
                            Right b'         -> gameLoop (nextColor color) b' Nothing
--                            Left  "Finished" -> putStrLn "Finished"  -- Exit
                            Left  msg        -> gameLoop color b (Just msg)

main = let board = readBoard initialBoard
       in gameLoop Chess.White board Nothing
