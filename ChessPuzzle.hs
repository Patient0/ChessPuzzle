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

-- to start with, a minimalist representation
-- for efficency we could later try leaving
-- some things already computed
data Board = Board { rows, columns :: Integer, free :: Squares, placements :: Placements} deriving Eq

-- We always keep pieces sorted
addPiece :: Placement -> Board -> Board
addPiece p (Board r c free ps) =
    let unoccupied = delete (fst p) free
    in
        (Board r c unoccupied (sort (p:ps)))

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

-- Get all free which are under attack
-- in the given board
underAttack :: Board -> Squares
underAttack (Board _ _ free ps) =
    [s | s <- free, (t,p) <- ps, allowedMove p s t]

-- Get all of the free from which this piece can attack
-- an existing piece on the board
canAttackFrom :: Board -> Piece -> Squares
canAttackFrom (Board _ _ free ps) piece =
    [s | s <- free, (t,_) <- ps, allowedMove piece s t]

available :: Board -> Squares
available b@(Board _ _ free _) =
    free \\ (underAttack b)

solutions :: Board -> [Piece] -> [Board]
solutions b [] = [b]
solutions b (p:ps) =
    let options = available b
        attackFrom = canAttackFrom b p
        placements = [(s,p) | s <- options \\ attackFrom]
        nextBoards = [addPiece p b | p <- placements]
        subs = [solutions nb ps | nb <- nextBoards]
    in
        nub $ concat subs

twoRooks = solutions (newBoard 2 2) [Rook, Rook]
eightQueens = solutions emptyBoard $ replicate 8 Queen

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
-- Efficiency!
-- List monad to cleanup syntax?
