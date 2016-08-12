module Validate where

import Control.Monad
import Data.Maybe
import Control.Monad.Fix

import Board

isValid :: Player -> ((Int, Int), Maybe BoardPiece) -> ((Int, Int), Maybe BoardPiece) -> Board -> Bool

--check that the player playing the right piece
isValid White (_, Just (Black, _)) _ _ = False
isValid Black (_, Just (White, _)) _ _ = False
--check that destination is not from the same color
isValid White _ (_, Just (White, _)) _ = False
isValid Black _ (_, Just (Black, _)) _ = False

-- call is valid for each piece
isValid _ (from, Just (c, Pawn)) (to, Just (cTarget, _)) board = isValidPawn c from to cTarget True board
isValid _ (from, Just (c, Pawn)) (to, Nothing) board = isValidPawn c from to c False board
isValid _ (from, Just (c, Rook)) (to, _) board = isValidRook c from to board
isValid _ (from, Just (c, Knight)) (to, _) board = isValidKnight c from to board
isValid _ (from, Just (c, Bishop)) (to, _) board = isValidBishop c from to board
isValid _ (from, Just (c, Queen)) (to, _) board = isValidQueen c from to board
isValid _ (from, Just (c, King)) (to, _) board = isValidKing c from to board
-- sink
isValid _ _ _ _ = False

-- check if player can move any piece. if not, it's probably checkmate
canMove :: Player -> (Int, Int) -> (Int, Int) -> Board -> Bool
canMove c from to board | from == (8, 8) = False
                        | to   == (8, 8) = canMove c (getNext from) (0, 0) board
                        | isValid c (readPiece from board) (readPiece to board) board = True
                        | otherwise = canMove c from (getNext to) board

-- check if player is in chess given the coordinate of its king.
-- isChess' running on all the board and try if any of the opposite player's piece threat the king.
isChess :: Player -> (Int, Int) -> Board -> Bool
isChess color king b = isChess' color king (readPiece (0,0) b) b
isChess' color king from b | fst from == (8, 8) = False
                           | isValid (nextColor color) from (king, Just(color, King)) b = True
                           | otherwise = isChess' color king (readPiece (getNext (fst from)) b) b

-- check if the path "from to" is clear - no other pieces on the way.
isClear :: (Int, Int) -> (Int, Int) -> Board -> Bool
isClear (x1, y1) (x2, y2) b | x1 == x2  = fix (\check y ->  -- forward/ backward
                                                    if y == y2 then True 
                                                    else (fromJust $ lookup (x1, y) b) == Nothing
                                                          && (if y1 < y2 then check (y + 1)
                                                              else check (y - 1)))
                                                                 (if y1 < y2 then (y1 + 1)
                                                                      else (y1 - 1))
                              | y1 == y2  = fix (\check x ->  -- right/ left
                                                    if x == x2 then True 
                                                    else (fromJust $ lookup (x, y1) b) == Nothing 
                                                          && (if x1 < x2 then check (x + 1) else check (x - 1))) 
                                                                (if x1 < x2 then (x1 + 1) else (x1 - 1))
                              | y1 < y2   = fix (\check (x, y) -> -- diagonal forward
                                                    if (x == x2 && y == y2) then True 
                                                    else (fromJust $ lookup (x, y) b) == Nothing 
                                                            && (if (x1 < x2) then check (x + 1, y + 1)
                                                                else check (x - 1, y + 1)))
                                                                    (if (x1 < x2) then (x1 + 1, y1 + 1)
                                                                        else (x1 - 1, y1 + 1))
                              | otherwise = fix (\check (x, y) -> -- diagonal backward
                                                    if (x == x2 && y == y2) then True 
                                                    else (fromJust $ lookup (x, y) b) == Nothing
                                                            && (if (x1 < x2) then check (x + 1, y - 1)
                                                                else check (x - 1, y - 1)))
                                                                    (if (x1 < x2) then (x1 + 1, y1 - 1)
                                                                        else (x1 - 1, y1 - 1))

-- | Pawn move
--   If dest is forward, then there is no piece or
--   If dest is diagonal, then there is an enemy piece
isValidPawn White (x1, 1) (x2, 3) _ False board = x1 == x2 && 
                                                  isClear (x1, 1) (x2, 3) board && 
                                                  not (isChess White (findKing White (0, 0) board) (afterMoveBoard White (x1, 1) (x2, 3) board))
isValidPawn Black (x1, 6) (x2, 4) _ False board = x1 == x2 && 
                                                  isClear (x1, 6) (x2, 3) board && 
                                                  not (isChess Black (findKing Black (0, 0) board) (afterMoveBoard Black (x1, 6) (x2, 4) board))
isValidPawn c from to _ False board             = (getForward c from) == to && 
                                                  not (isChess c (findKing c (0, 0) board) (afterMoveBoard c from to board))
isValidPawn c from to cTarget True board        = to `elem` (getForwardDiagonals c from) && 
                                                  c /= cTarget && 
                                                  not (isChess c (findKing c (0, 0) board) (afterMoveBoard c from to board))

-- | Rook move
--   check if dest is "stright line" and the line is clear
isValidRook c (x1, y1) (x2, y2) board = ((x1 == x2 && y1 /= y2) || (x1 /= x2 && y1 == y2)) && 
                                        isClear (x1, y1) (x2, y2) board && 
                                        not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))

-- | Bishop move
--   check if dest is "diagonal line" and the line is clear
isValidBishop c (x1, y1) (x2, y2) board = (abs (x1 - x2) == abs (y1 - y2)) && 
                                          isClear (x1, y1) (x2, y2) board && 
                                          not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))

-- | Queen move
--   check if dest is "diagonal line" or "stright line" and the line is clear
isValidQueen c (x1, y1) (x2, y2) board | x1 == x2 = isClear (x1, y1) (x2, y2) board && 
                                                    not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
                                       | y1 == y2 = isClear (x1, y1) (x2, y2) board && 
                                                    not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
                                       | abs (x1 - x2) == abs (y1 - y2) = isClear (x1, y1) (x2, y2) board && 
                                                                          not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
                                       | otherwise = False
                                       
-- | King move
--   check if dest is within one "step" on each vertex
isValidKing c (x1, y1) (x2, y2) board = (abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1) && 
                                        isClear (x1, y1) (x2, y2) board && 
                                        not (isChess c (x2, y2) board)

-- | Knight move
--   check if dest is knight move. not necessery to check if the way is clear
isValidKnight c (x1, y1) (x2, y2) board = ((abs (x1 - x2) == 1 && abs (y1 - y2) == 2) || (abs (x1 - x2) == 2 && abs (y1 - y2) == 1)) && 
                                          not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
