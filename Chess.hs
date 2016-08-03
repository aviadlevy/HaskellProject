module Chess where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.Fix

-- Data and type defines

data Player     = White 
                | Black 
                deriving (Show, Eq)

nextColor Black = White
nextColor White = Black

data Piece      = Pawn
                | Rook
                | Bishop
                | Knight
                | King
                | Queen
                deriving (Show, Eq)

type BoardPiece = (Player, Piece) 
type BoardEntry = ((Int, Int), Maybe BoardPiece) 
type Board      = [BoardEntry]

-- The initial state of the game

initialBoard :: String
initialBoard = unlines ["rnbqkbnr"
                       ,"pppppppp"
                       ,"........"
                       ,"........"
                       ,"........"
                       ,"........"
                       ,"PPPPPPPP"
                       ,"RNBQKBNR"
                       ]


readBoard   :: String -> Board
readBoard s = zip coords pieces
              where coords    = [(x,y) | y <- [0..7] , x <- [0..7]] 
                    pieces    = map readSquare $ filter (/= '\n') s

writeBoard   :: Board -> String 
writeBoard b = "  ABCDEFGH\n" ++ (format 1 $ map writeSquareUnicode $ map snd b) ++ "  ABCDEFGH"
               where format i [] = []
                     format i xs = (show i) ++ " " ++ (fst s) ++ "\n" ++ (format (i+1) $ snd s)
                        where s = splitAt 8 xs

readSquare  :: Char -> Maybe BoardPiece
readSquare c = 
    case c of 
        'R' -> Just (Black, Rook)
        'N' -> Just (Black, Knight)
        'B' -> Just (Black, Bishop)
        'K' -> Just (Black, King)
        'Q' -> Just (Black, Queen)
        'P' -> Just (Black, Pawn)
        'r' -> Just (White, Rook)
        'n' -> Just (White, Knight)
        'b' -> Just (White, Bishop)
        'k' -> Just (White, King)
        'q' -> Just (White, Queen)
        'p' -> Just (White, Pawn)
        otherwise -> Nothing

writeSquareUnicode  :: Maybe BoardPiece -> Char
writeSquareUnicode p =
    case p of 
        Just (Black, Rook)   -> '\9814'
        Just (Black, Knight) -> '\9816'
        Just (Black, Bishop) -> '\9815'
        Just (Black, King)   -> '\9812'
        Just (Black, Queen)  -> '\9813'
        Just (Black, Pawn)   -> '\9817'
        Just (White, Rook)   -> '\9820'
        Just (White, Knight) -> '\9822'
        Just (White, Bishop) -> '\9821'
        Just (White, King)   -> '\9818'
        Just (White, Queen)  -> '\9819'
        Just (White, Pawn)   -> '\9823'
        otherwise -> '.'

setPiece            :: Player -> BoardEntry -> (Int, Int) -> Board -> Board 
setPiece White ((x, 6), Just (White, Pawn)) loc b =  setPiece White ((x, 6), Just (White, Queen)) loc b
setPiece Black ((x, 1), Just (Black, Pawn)) loc b =  setPiece Black ((x, 1), Just (Black, Queen)) loc b
setPiece p pc loc b = let i = fromJust $ elemIndex (loc, fromJust $ lookup loc b) b
                          (x,_:ys) = splitAt i b 
                      in x ++ [(loc, snd pc)] ++ ys
                      
isValid :: Player -> ((Int, Int), Maybe BoardPiece) -> ((Int, Int), Maybe BoardPiece) -> Board -> Bool
--check that the player playing the right piece
isValid White (_, Just (Black, _)) _ _ = False
isValid Black (_, Just (White, _)) _ _ = False
--check that destination is not from the same color
isValid White _ (_, Just (White, _)) _ = False
isValid Black _ (_, Just (Black, _)) _ = False


isValid _ (from, Just (c, Pawn)) (to, Just (cTarget, _)) board = isValidPawn c from to cTarget True board
isValid _ (from, Just (c, Pawn)) (to, Nothing) board = isValidPawn c from to c False board
isValid _ (from, Just (c, Rook)) (to, _) board = isValidRook c from to board
isValid _ (from, Just (c, Knight)) (to, _) board = isValidKnight c from to board
isValid _ (from, Just (c, Bishop)) (to, _) board = isValidBishop c from to board
isValid _ (from, Just (c, Queen)) (to, _) board = isValidQueen c from to board
isValid _ (from, Just (c, King)) (to, _) board = isValidKing c from to board

isValid _ _ _ _ = False

canMove c from to board | from == (8, 8) = False
                        | to   == (8, 8) = canMove c (getNext from) (0, 0) board
                        | isValid c (readPiece from board) (readPiece to board) board = True
                        | otherwise = canMove c from (getNext to) board
                        
afterMoveBoard p from to b = b''
                             where piece  = fromJust $ lookup from b
                                   source = (from, piece)
                                   piece' = fromJust $ lookup to b
                                   target = (to, piece')
                                   b'     = setPiece p (from, piece) to b
                                   b''    = setPiece p (to, Nothing) from b'

readPiece :: (Int, Int) -> Board -> BoardEntry
readPiece (x, y) b = ((x, y), fromJust $ lookup (x, y) b)

findKing :: Player -> (Int, Int) -> Board -> (Int, Int)
findKing color (x, y) b | snd (readPiece (x, y) b) == Just (color, King) = fst (readPiece (x, y) b)
                        | otherwise = findKing color (getNext (x, y)) b

isChess color king b = isChess' color king (readPiece (0,0) b) b

isChess' color king from b | fst from == (8, 8) = False
                           | isValid (nextColor color) from (king, Just(color, King)) b = True
                           | otherwise = isChess' color king (readPiece (getNext (fst from)) b) b
                           
getNext (x, y) | (x, y) == (7, 7) = (8, 8)
               | x == 7 = (0, y + 1)
               | otherwise = (x + 1, y)

getForward Black (x, y) = (x, y - 1)
getForward White (x, y) = (x, y + 1)
getForwardDiagonals c l@(x, y) = (left, right)
                                 where (fx, fy) = getForward c l
                                       left     = (fx - 1, fy)
                                       right    = (fx + 1, fy)


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
isValidPawn White (x1, 1) (x2, 3) _ False board = x1 == x2 && isClear (x1, 1) (x2, 3) board && not (isChess White (findKing White (0, 0) board) (afterMoveBoard White (x1, 1) (x2, 3) board))
isValidPawn Black (x1, 6) (x2, 4) _ False board = x1 == x2 && isClear (x1, 6) (x2, 3) board && not (isChess Black (findKing Black (0, 0) board) (afterMoveBoard Black (x1, 6) (x2, 4) board))
isValidPawn c from to _ False board                 = (getForward c from) == to && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c from to board))
isValidPawn c from to cTarget True board            = to `elem` (getForwardDiagonals c from) && c /= cTarget && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c from to board))

-- | Rook move
--   check if dest is "stright line" and the line is clear
isValidRook c (x1, y1) (x2, y2) board = ((x1 == x2 && y1 /= y2) || (x1 /= x2 && y1 == y2)) && isClear (x1, y1) (x2, y2) board && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))

-- | Bishop move
--   check if dest is "diagonal line" and the line is clear
isValidBishop c (x1, y1) (x2, y2) board = (abs (x1 - x2) == abs (y1 - y2)) && isClear (x1, y1) (x2, y2) board && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))

-- | Queen move
--   check if dest is "diagonal line" or "stright line" and the line is clear
isValidQueen c (x1, y1) (x2, y2) board | x1 == x2                       = isClear (x1, y1) (x2, y2) board && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
                                       | y1 == y2                       = isClear (x1, y1) (x2, y2) board && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
                                       | abs (x1 - x2) == abs (y1 - y2) = isClear (x1, y1) (x2, y2) board && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
                                       | otherwise                      = False
                                       
-- | King move
--   check if dest is "diagonal line" and the line is clear
isValidKing c (x1, y1) (x2, y2) board = (abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1) && isClear (x1, y1) (x2, y2) board && not (isChess c (x2, y2) board)

-- | Knight move
--   check if dest is knight move. not necessery to check if the way is clear
isValidKnight c (x1, y1) (x2, y2) board = ((abs (x1 - x2) == 1 && abs (y1 - y2) == 2) || (abs (x1 - x2) == 2 && abs (y1 - y2) == 1)) && not (isChess c (findKing c (0, 0) board) (afterMoveBoard c (x1, y1) (x2, y2) board))
