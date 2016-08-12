module Board where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad

data Player     = White 
                | Black 
                deriving (Show, Eq)

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

-- get next player
nextColor :: Player -> Player
nextColor Black = White
nextColor White = Black

-- The initial state of the game. 
-- using unlines to join the lines to one longer string.
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

-- get String and convert it to Board. 
-- using zip to create BoardPiece, and map to map each square from the char letter to Piece type.
readBoard :: String -> Board
readBoard s = zip coords pieces
              where coords    = [(x,y) | y <- [0..7] , x <- [0..7]] 
                    pieces    = map readSquare $ filter (/= '\n') s

-- write board to screen.
-- using map to conver from Piece to unicode representation. splitAt 8 to deal with each line independently.
writeBoard :: Board -> String 
writeBoard b = "  ABCDEFGH\n" ++ (format 1 $ map writeSquareUnicode $ map snd b) ++ "  ABCDEFGH"
               where format i [] = []
                     format i xs = (show i) ++ " " ++ (fst s) ++ "\n" ++ (format (i+1) $ snd s)
                        where s = splitAt 8 xs

readSquare :: Char -> Maybe BoardPiece
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

writeSquareUnicode :: Maybe BoardPiece -> Char
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

-- set piece from a location to a new location, and retur new board.
-- special case: converting pawn to queen when reaching the other side of the board.
-- i is the new location in the board. then split the old board at i and copy the Piece to the new location (NOT deleting the old one. need to do it yourself!!)
setPiece :: Player -> BoardEntry -> (Int, Int) -> Board -> Board 
setPiece White ((x, 6), Just (White, Pawn)) loc b =  setPiece White ((x, 6), Just (White, Queen)) loc b
setPiece Black ((x, 1), Just (Black, Pawn)) loc b =  setPiece Black ((x, 1), Just (Black, Queen)) loc b
setPiece p pc loc b = let i = fromJust $ elemIndex (loc, fromJust $ lookup loc b) b
                          (x,_:ys) = splitAt i b 
                      in x ++ [(loc, snd pc)] ++ ys

readPiece :: (Int, Int) -> Board -> BoardEntry
readPiece (x, y) b = ((x, y), fromJust $ lookup (x, y) b)

-- get the new board after a move "from to"
afterMoveBoard :: Player -> (Int, Int) -> (Int, Int) -> Board -> Board
afterMoveBoard p from to b = b''
                             where piece  = fromJust $ lookup from b
                                   source = (from, piece)
                                   piece' = fromJust $ lookup to b
                                   target = (to, piece')
                                   b'     = setPiece p (from, piece) to b
                                   b''    = setPiece p (to, Nothing) from b'

-- find the king of the given Player.
findKing :: Player -> (Int, Int) -> Board -> (Int, Int)
findKing color (x, y) b | snd (readPiece (x, y) b) == Just (color, King) = fst (readPiece (x, y) b)
                        | otherwise = findKing color (getNext (x, y)) b

-- get the next coordinate on Board
getNext :: (Int, Int) -> (Int, Int)
getNext (x, y) | (x, y) == (7, 7) = (8, 8)
               | x == 7 = (0, y + 1)
               | otherwise = (x + 1, y)

getForward :: Player -> (Int, Int) -> (Int, Int)
getForward Black (x, y) = (x, y - 1)
getForward White (x, y) = (x, y + 1)

getForwardDiagonals :: Player -> (Int, Int) -> ((Int, Int), (Int, Int))
getForwardDiagonals c l@(x, y) = (left, right)
                                 where (fx, fy) = getForward c l
                                       left     = (fx - 1, fy)
                                       right    = (fx + 1, fy)
