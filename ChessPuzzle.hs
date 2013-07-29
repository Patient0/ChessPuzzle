module ChessPuzzle where
import Data.List
import Control.Monad
import Data.Map hiding (lookup, foldr, map)

-- Square is a row and a column
type Square = (Int, Int)
-- A move is a relative offset to a square
type Move = (Int, Int)
-- Squares is a list of free squares
type Squares = [Square]

data Piece  = Pawn | Rook | Knight | Bishop | Queen | King deriving (Read, Show, Eq, Ord, Enum, Bounded)

-- This function defines what moves each piece can make
canMove :: Piece -> Move -> Bool
canMove Pawn move = elem move [(1,-1), (1,1)]
canMove Rook (r,c) = r /= c && (r == 0 || c == 0)
canMove Knight (r,c) = elem (abs(r), abs(c)) [(1,2), (2,1)]
canMove Bishop (r,c) = r /= 0 && abs(r) == abs(c)
canMove Queen move = canMove Bishop move || canMove Rook move
canMove King move = elem move [(r, c) | r <- [-1,0,1], c <- [-1,0,1], (r,c) /= (0,0)]

-- Symbolic representation of each piece, for board display
pieceToChar :: Piece -> Char
pieceToChar Pawn = 'P'
pieceToChar Rook = 'R'
pieceToChar Knight = 'N'
pieceToChar Bishop = 'B'
pieceToChar Queen = 'Q'
pieceToChar King = 'K'

pieces = [Pawn .. King]
pieceChars = map pieceToChar pieces

charToPiece :: Char -> Maybe Piece
charToPiece c =
    let chars = map pieceToChar pieces
    in
        lookup c $ zip chars pieces

toPiece :: Char -> Piece
toPiece c =
    case (charToPiece c) of
        Just p -> p
        Nothing -> error $ "No piece defined for character '" ++ [c] ++ "'"

-- Convert a string like 'QNR' into [Queen, Knight, Rook]
toPieces :: [Char] -> [Piece]
toPieces = map toPiece

-- diff gets the move that relates two squares
diff :: Square -> Square -> Move
diff (x2, y2) (x1, y1) = (x1-x2, y1-y2)

-- return if a given piece is allowed to move
-- from the first square to the second square
allowedMove :: Piece -> Square -> Square -> Bool
allowedMove p from to = canMove p (diff from to)

-- A placement indicates that a certain piece is on a
-- particular square on the board
type Placement = (Square, Piece)
type Placements = [Placement]

-- 'free' is redundant in that it can be calculated from scratch
-- from the placements and dimensions. However, for efficiency
-- we keep this along with the board to save recomputing.
data Board = Board {
    rows, columns :: Int, -- The dimensions of the board
    free :: Squares, -- squares which are not in control of any pieces
    placements :: Placements -- the placements of each piece on the board
    }

-- Return the board that results from adding a
-- piece to the board at a given position.
-- This means:
--  adding the new placement to the list of placements
--  removing the squares that the new piece controls from 'free'
addPiece :: Placement -> Board -> Board
addPiece pl@(location,piece) (Board r c free pls) =
    let allowed = allowedMove piece location
        newFree = [s | s <- free, s /= location, not (allowed s)]
    in
        Board r c newFree (pl:pls)


emptySquares :: Int -> Int -> Squares
emptySquares rows columns =
    [(r,c) | r <- [1..rows], c <- [1..columns]]

-- Create a new board with no pieces
emptyBoard :: Int -> Int -> Board
emptyBoard rows columns =
    Board rows columns (emptySquares rows columns) []

-- A standard chess board
standardBoard = emptyBoard 8 8

toChar :: Maybe Piece -> Char
toChar (Just p) = pieceToChar p
toChar Nothing = '.'

instance Show Board where
    show (Board rows columns _ placements) =
        let buildRow r = "|" ++ [toChar (lookup (r,c) placements) | c <- [1..columns]] ++ "|\n"
            bar = "+" ++ (replicate columns '-') ++ "+\n"
            rowStrings = map buildRow [1..rows]
        in
            bar ++
            (concat rowStrings) ++
            bar

-- The list of squares that are occupied on a board
occupied :: Board -> Squares
occupied b = [s | (s,_) <- placements b]

-- Can the piece 'p' attack anything on the board
-- from square 's'?
canAttackFrom :: Board -> Piece -> Square -> Bool
canAttackFrom b p s =
    any (allowedMove p s) $ occupied b

-- Take a list of boards and filter out the ones with
-- duplicate placements
uniqueBoards :: [Board] -> [Board]
uniqueBoards bs =
    let addBoard b = Data.Map.insert (sort $ placements b) b
    in
        elems $ foldr addBoard empty bs

-- If the pieces are sorted, we only need to
-- filter out duplicates if the most recently added piece is
-- the same as the last one added. This optimization
-- is important to save us from keeping 20 million
-- potential chess boards "in memory" just to detect
-- duplicates.
ensureUnique :: [Piece] -> [Board] -> [Board]
ensureUnique (p1:p2:ps) | p1 == p2 = uniqueBoards
ensureUnique _ = id

-- The main part of the algorithm:
--  For each of the solutions placing N-1 pieces,
--   work out all of the ways in which the next piece can be added.
--  Remove any duplicates.
solutions :: Board -> [Piece] -> [Board]
solutions initial [] = [initial]
solutions initial pieces@(p:ps) =
    let nonUnique = do
        b <- solutions initial ps
        s <- free b
        guard $ not $ canAttackFrom b p s
        return $ addPiece (s,p) b
    in
        ensureUnique pieces nonUnique

-- Main entry point to this module.
-- Ensures that pieces is sorted (required for
-- 'unique' optimization).
chess :: Int -> Int -> [Piece] -> [Board]
chess rows columns pieces =
    solutions (emptyBoard rows columns) (sort pieces)

-- A very simple test case
twoRooks = chess 2 2 $ [Rook, Rook]
-- The example problems in the document
example1 = chess 3 3 [Rook, King, King]
example2 = chess 4 4 ((replicate 2 Rook) ++ (replicate 4 Knight))
-- length test == 20136752
test = chess 6 9 [Queen, Rook, Bishop, Knight, King, King]
-- The 'classic' problem of 8 queens on a standard chess board
eightQueens = chess 8 8 (replicate 8 Queen)
