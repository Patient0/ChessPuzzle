module ChessPuzzle where
import Data.List
-- Square is a row and a column
type Square = (Integer, Integer)
-- A move is a relative offset to a square
type Move = (Integer, Integer)
-- Squares is a list of free
type Squares = [Square]

data Piece  = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq, Ord)

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

emptySquares :: Integer -> Integer -> Squares
emptySquares rows columns =
    [(r,c) | r <- [1..rows], c <- [1..columns]]

type Placement = (Square, Piece)
type Placements = [Placement]

data Board = Board { rows, columns :: Integer, free :: Squares, placements :: Placements} deriving Eq

addPiece :: Placement -> Board -> Board
addPiece pl@(location,piece) (Board r c free pls) =
    let allowed = allowedMove piece location
        newFree = [s | s <- free, s /= location, not (allowed s)]
    in
        Board r c newFree (sort (pl:pls))

newBoard :: Integer -> Integer -> Board
newBoard rows columns =
    Board rows columns (emptySquares rows columns) []

emptyBoard = newBoard 8 8

liftChar :: Maybe Char -> Char
liftChar (Just x) = x
liftChar Nothing = '.'

-- A 2D grid amenable as display as text
data GridDisplay = GridDisplay Integer Integer [(Square, Char)]

instance Show GridDisplay where
    show (GridDisplay rows columns placements) =
        let buildRow r = [liftChar (lookup (r,c) placements) | c <- [1..columns]]
            rowStrings = map buildRow [1..rows]
        in
            intercalate "\n" rowStrings

instance Show Board where
    show (Board rows columns free placements) =
        show (GridDisplay rows columns [(k, pieceToChar p) | (k,p) <- placements])

-- Can the piece 'p' attack anything on the board
-- from square 's'?
canAttackFrom :: Board -> Piece -> Square -> Bool
canAttackFrom (Board _ _ free ps) p s =
    any (allowedMove p s) [t | (t,_) <- ps]

solutions :: Board -> [Piece] -> [Board]
solutions b [] = [b]
solutions b (p:ps) =
    let placements = [(s,p) | s <- free b, not (canAttackFrom b p s)]
        nextBoards = [addPiece p b | p <- placements]
        subs = [solutions nb ps | nb <- nextBoards]
    in
        nub $ concat subs

twoRooks = solutions (newBoard 2 2) [Rook, Rook]
eightQueens = solutions emptyBoard $ replicate 8 Queen
example1 = solutions (newBoard 3 3) [Rook, King, King]
example2 = solutions (newBoard 4 4) ((replicate 2 Rook) ++ (replicate 4 Knight))
-- length test == 20136752
test = solutions (newBoard 6 9) [Queen, Rook, Bishop, Knight, King, King]

addPieces :: Board -> Placements -> Board
addPieces b ps =
    foldr addPiece b ps

maxColumn :: [Square] -> Integer
maxColumn = maximum . (map snd)
maxRow :: [Square] -> Integer
maxRow = maximum . (map fst)

showSquares :: Squares -> GridDisplay
showSquares sqs =
    GridDisplay (maxRow sqs) (maxColumn sqs) [(s,'O') | s <- sqs]
 
-- Next steps
-- Filter duplicates more efficiently
--  we only need to worry about the case of two consecutive pieces that
--  are the same. The rest should fall out from there.
-- List monad to cleanup syntax?
-- IO and polishing.
