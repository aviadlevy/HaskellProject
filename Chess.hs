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
                    pieces    = map readSquare $ filter (/= '\n') s

writeBoard   :: Board -> String 
writeBoard b = "  ABCDEFGH\n" ++ (format 1 $ map writeSquareUnicode $ map snd b) 
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
setPiece p pc loc b = let i = fromJust $ elemIndex (loc, fromJust $ lookup loc b) b
                          (x,_:ys) = splitAt i b 
                      in x ++ [(loc, snd pc)] ++ ys

--getPiece :: (Int, Int) -> Board -> BoardEntry
--getPiece (x, y) b = b !! (x * 8 + y)

isValid White (_, Just (Black, _)) _ _ = False
isValid Black (_, Just (White, _)) _ _ = False

isValid White (from, Just (c, Pawn)) (to, Just (Black, targetPiece)) board = isValidPawn c from to True board
isValid Black (from, Just (c, Pawn)) (to, Just (White, targetPiece)) board = isValidPawn c from to True board
isValid player (from, Just (c, Pawn)) (to, Nothing) board = isValidPawn c from to False board
--isValid (from, Just (c, Rook)) to board = isValidRook c from to board
--isValid _ _ _ = False

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
isValidPawn White (x1, 1) (x2, 3) False b = x1 == x2 && (fromJust $ lookup (x1, 2) b) == Nothing
isValidPawn Black (x1, 6) (x2, 4) False b = x1 == x2 && (fromJust $ lookup (x1, 5) b) == Nothing
isValidPawn _ (x1, y1) (x2, y2) False b = abs (y1 - y2) == 1
--isValidPawn _ _ _ _ = False
