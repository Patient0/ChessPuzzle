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

newBoard :: Integer -> Integer -> Board
newBoard rows columns =
    Board (emptySquares rows columns) []

standardBoard = newBoard 8 8

type Placement = (Square, Piece)
type Placements = [Placement]

-- to start with, a minimalist representation
-- for efficency we could later try leaving
-- some things already computed
data Board = Board Squares Placements

instance Show Board where
    show (Board squares placements) =
        let columns = maximum $ map snd squares
            rows = maximum $ map fst squares
            buildRow r = [letter (lookup (r,x) placements) | x <- [1..columns]]
            rowStrings = [buildRow r | r <- [1..rows]]
        in
            intercalate "\n" rowStrings

-- squares takes:
-- * a placement
-- * a list of available squares
-- returns list of squares that can be moved to
squares :: Placement -> Squares -> Squares
squares (s,p) = filter (allowedMove p s)

type Solution = [Placement]
solutions :: Squares -> [Piece] -> [Solution]
-- empty board has no solutions
solutions [] _ = []
-- only one piece left has a solution for each possible
-- square that the piece could be
solutions b [p] = [[(s,p) | s <- b]]
-- continue here...
solutions b (p:ps) = []

main :: IO ()
main = return ()
-- King :: Piece
-- Queen :: Piece
-- Pawn :: Piece
-- Knight :: Piece
-- Rook :: Piece

