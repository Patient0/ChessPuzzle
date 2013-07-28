module ChessPuzzle where
import Data.List
import Control.Monad
import Data.Map hiding (lookup, foldr, map)

-- Square is a row and a column
type Square = (Int, Int)
-- A move is a relative offset to a square
type Move = (Int, Int)
-- Squares is a list of free
type Squares = [Square]

data Piece  = Pawn | Rook | Knight | Bishop | Queen | King deriving (Read, Show, Eq, Ord)

canMove :: Piece -> Move -> Bool
canMove Pawn move = elem move [(1,-1), (1,1)]
canMove Rook (r,c) = r /= c && (r == 0 || c == 0)
canMove Knight (r,c) = elem (abs(r), abs(c)) [(1,2), (2,1)]
canMove Bishop (r,c) = r /= 0 && abs(r) == abs(c)
canMove Queen move = canMove Bishop move || canMove Rook move
canMove King move = elem move [(r, c) | r <- [-1,0,1], c <- [-1,0,1], (r,c) /= (0,0)]

pieceToChar :: Piece -> Char
pieceToChar Pawn = 'P'
pieceToChar Rook = 'R'
pieceToChar Knight = 'N'
pieceToChar Bishop = 'B'
pieceToChar Queen = 'Q'
pieceToChar King = 'K'

-- diff gets the move that relates two free
diff :: Square -> Square -> Move
diff (x2, y2) (x1, y1) = (x1-x2, y1-y2)

-- return if a given piece is allowed to move
-- from the first square to the second square
allowedMove :: Piece -> Square -> Square -> Bool
allowedMove p from to = canMove p (diff from to)

emptySquares :: Int -> Int -> Squares
emptySquares rows columns =
    [(r,c) | r <- [1..rows], c <- [1..columns]]

type Placement = (Square, Piece)
type Placements = [Placement]

data Board = Board { rows, columns :: Int, free :: Squares, placements :: Placements}

addPiece :: Placement -> Board -> Board
addPiece pl@(location,piece) (Board r c free pls) =
    let allowed = allowedMove piece location
        newFree = [s | s <- free, s /= location, not (allowed s)]
    in
        Board r c newFree (pl:pls)

newBoard :: Int -> Int -> Board
newBoard rows columns =
    Board rows columns (emptySquares rows columns) []

emptyBoard = newBoard 8 8

liftChar :: Maybe Char -> Char
liftChar (Just x) = x
liftChar Nothing = '.'

-- A 2D grid amenable for display as text
data GridDisplay = GridDisplay Int Int [(Square, Char)]

instance Show GridDisplay where
    show (GridDisplay rows columns placements) =
        let buildRow r = "|" ++ [liftChar (lookup (r,c) placements) | c <- [1..columns]] ++ "|\n"
            header = "+" ++ (replicate columns '-') ++ "+\n"
            footer = header ++ "\n"
            rowStrings = map buildRow [1..rows]
        in
            header ++
            (concat rowStrings) ++
            footer

instance Show Board where
    show (Board rows columns free placements) =
        show (GridDisplay rows columns [(k, pieceToChar p) | (k,p) <- placements])

-- The list of squares that are occupied on a board
occupied :: Board -> Squares
occupied b = [s | (s,_) <- placements b]

-- Can the piece 'p' attack anything on the board
-- from square 's'?
canAttackFrom :: Board -> Piece -> Square -> Bool
canAttackFrom b p s =
    any (allowedMove p s) $ occupied b

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

chess :: Int -> Int -> [Piece] -> [Board]
chess rows columns pieces =
    solutions (newBoard rows columns) (sort pieces)

twoRooks = chess 2 2 $ [Rook, Rook]
eightQueens = chess 8 8 (replicate 8 Queen)
example1 = chess 3 3 [Rook, King, King]
example2 = chess 4 4 ((replicate 2 Rook) ++ (replicate 4 Knight))
-- length test == 20136752
test = chess 6 9 [Queen, Rook, Bishop, Knight, King, King]

showSquares :: Squares -> GridDisplay
showSquares sqs =
    let max f = maximum . (map f)
    in
        GridDisplay (max fst sqs) (max snd sqs) [(s,'O') | s <- sqs]
 
-- Next steps
-- IO and polishing.
