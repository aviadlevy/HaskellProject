module Chess where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char

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

-- The initial state of the game, black on top

initialBoard :: String
initialBoard = unlines ["rnbkqbnr"
                       ,"pppppppp"
                       ,"........"
                       ,"........"
                       ,"........"
                       ,"........"
                       ,"PPPPPPPP"
                       ,"RNBQKBNR"
                       ]

-- Read and write functions

readBoard   :: String -> Board
readBoard s = zip coords pieces
              where coords    = [(x,y) | y <- [0..7] , x <- [0..7]] 
                    pieces    = map readSquare $ filter (/= '\n') initialBoard

writeBoard   :: Board -> String 
writeBoard b = "  ABCDEFGH\n" ++ (format 1 $ map writeSquareUnicode $ map snd b) 
               where format i [] = []
                     format i xs = (show i) ++ " " ++ (fst s) ++ "\n" ++ (format (i+1) $ snd s)
                        where s = splitAt 8 xs

readSquare  :: Char -> Maybe BoardPiece
readSquare c = 
    case c of 
        'r' -> Just (Black, Rook)
        'n' -> Just (Black, Knight)
        'b' -> Just (Black, Bishop)
        'k' -> Just (Black, King)
        'q' -> Just (Black, Queen)
        'p' -> Just (Black, Pawn)
        'R' -> Just (White, Rook)
        'N' -> Just (White, Knight)
        'B' -> Just (White, Bishop)
        'K' -> Just (White, King)
        'Q' -> Just (White, Queen)
        'P' -> Just (White, Pawn)
        otherwise -> Nothing

-- Inverse of readSquare
writeSquare  :: Maybe BoardPiece -> Char
writeSquare p = 
    case p of 
        Just (Black, Rook)   -> 'r'
        Just (Black, Knight) -> 'n'
        Just (Black, Bishop) -> 'b'
        Just (Black, King)   -> 'k'
        Just (Black, Queen)  -> 'q'
        Just (Black, Pawn)   -> 'p'
        Just (White, Rook)   -> 'R'
        Just (White, Knight) -> 'N'
        Just (White, Bishop) -> 'B'
        Just (White, King)   -> 'K'
        Just (White, Queen)  -> 'Q'
        Just (White, Pawn)   -> 'P'
        otherwise -> '.'

writeSquareUnicode  :: Maybe BoardPiece -> Char
writeSquareUnicode p = 
    case p of 
        Just (Black, Rook)   -> '\9820'  
        Just (Black, Knight) -> '\9822' 
        Just (Black, Bishop) -> '\9821' 
        Just (Black, King)   -> '\9818' 
        Just (Black, Queen)  -> '\9819' 
        Just (Black, Pawn)   -> '\9823'
        Just (White, Rook)   -> '\9814' 
        Just (White, Knight) -> '\9816'
        Just (White, Bishop) -> '\9815' 
        Just (White, King)   -> '\9812' 
        Just (White, Queen)  -> '\9813' 
        Just (White, Pawn)   -> '\9817' 
        otherwise -> '.'

setPiece            :: Player -> BoardEntry -> (Int, Int) -> Board -> Board 
setPiece p pc loc b = let i = fromJust $ elemIndex (loc, fromJust $ lookup loc b) b
                          (x,_:ys) = splitAt i b 
                      in x ++ [(loc, snd pc)] ++ ys

--getPiece :: (Int, Int) -> Board -> BoardEntry
--getPiece (x, y) b = b !! (x * 8 + y)

isValid (from, Just (c, Pawn)) to board = isValidPawn c from to board
--isValid (from, Just (c, Rook)) to board = isValidRook c from to board
isValid _ _ _ = False

getForward Black (x, y) = (x, y + 1)
getForward White (x, y) = (x, y - 1)
getBackward c l         = getForward (nextColor c) l
getLeft  (x, y)         = (x - 1, y)
getRight (x, y)         = (x + 1, y)
getAdjacent l           = [getLeft l, getForward Black l, getRight l, getBackward Black l]
getForwardDiagonals c l@(x, y) = [left, right]
                                 where (fx, fy) = getForward c l
                                       left     = (fx - 1, fy)
                                       right    = (fx + 1, fy)
getBackwardDiagonals c l = getForwardDiagonals (nextColor c) l

-- | Pawn move
--   If dest is in bounds and
--   If dest is forward, then there is no piece or
--   If dest is diagonal, then there is an enemy piece
isValidPawn c from to b = False
