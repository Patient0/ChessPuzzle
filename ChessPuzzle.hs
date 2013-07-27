module ChessPuzzle where
import Data.List
-- Square is a row and a column
type Square = (Integer, Integer)
-- A move is a relative offset to a square
type Move = (Integer, Integer)
-- Squares is a list of squares
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

letter :: Maybe Piece -> Char
letter (Just p) = pieceToChar p
letter Nothing = '.'

-- diff gets the move that relates two squares
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
data Board = Board { squares :: Squares, placements :: Placements}

-- Ugly and inefficient but it'll do for now
instance Eq Board where
    (Board _ p1) == (Board _ p2) =
        (sort p1) == (sort p2)

instance Ord Board where
    (Board _ p1) < (Board _ p2) =
        (sort p1) < (sort p2)

newBoard :: Integer -> Integer -> Board
newBoard rows columns =
    Board (emptySquares rows columns) []

emptyBoard = newBoard 8 8

liftChar :: Maybe Char -> Char
liftChar (Just x) = x
liftChar Nothing = '.'

-- A chess board representationn amenable to
-- display as text
data DisplayBoard = DisplayBoard Integer Integer [(Square, Char)]

instance Show DisplayBoard where
    show (DisplayBoard rows columns placements) =
        let buildRow r = [liftChar (lookup (r,c) placements) | c <- [1..columns]]
            rowStrings = map buildRow [1..rows]
        in
            intercalate "\n" rowStrings

maxColumn :: [Square] -> Integer
maxColumn = maximum . (map snd)
maxRow :: [Square] -> Integer
maxRow = maximum . (map fst)

displayBoard :: [Square] -> [(Square, Char)] -> DisplayBoard
displayBoard squares placements =
    DisplayBoard (maxRow squares) (maxColumn squares) placements

instance Show Board where
    show (Board squares placements) =
        show (displayBoard squares [(k, pieceToChar p) | (k,p) <- placements])

addPiece :: Placement -> Board -> Board
addPiece p (Board squares ps) =
    (Board squares (p:ps))

addPieces :: Board -> Placements -> Board
addPieces b ps =
    foldr addPiece b ps

sshow :: Squares -> DisplayBoard
sshow squares =
    displayBoard squares [(s,'O') | s <- squares]

taken :: Board -> Squares
taken (Board _ ps) =
    map fst ps

-- Get all squares which are under attack
-- in the given board
underAttack :: Board -> Squares
underAttack (Board squares ps) =
    [s | s <- squares, (t,p) <- ps, allowedMove p s t]

-- Get all of the squares from which this piece can attack
-- an existing piece on the board
canAttackFrom :: Board -> Piece -> Squares
canAttackFrom (Board squares ps) piece =
    [s | s <- squares, (t,_) <- ps, allowedMove piece s t]

available :: Board -> Squares
available b@(Board squares _) =
    (squares \\ (taken b)) \\ (underAttack b)

nonUniqueSolutions :: Board -> [Piece] -> [Board]
nonUniqueSolutions b [] = [b]
nonUniqueSolutions b (p:ps) =
    let options = available b
        attackFrom = canAttackFrom b p
        placements = [(s,p) | s <- options \\ attackFrom]
        nextBoards = [addPiece p b | p <- placements]
        subs = [solutions nb ps | nb <- nextBoards]
    in
        concat subs

solutions :: Board -> [Piece] -> [Board]
solutions b ps = nub $ nonUniqueSolutions b ps

twoRooks = solutions (newBoard 2 2) [Rook, Rook]
 
-- Next steps
-- Efficiency!
-- List monad to cleanup syntax?

putLn :: (Show x) => x -> IO ()
putLn = putStrLn . show

main :: IO ()
main =
    do
    putLn twoRooks
    return ()
