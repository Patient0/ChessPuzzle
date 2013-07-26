module ChessPuzzle where
-- Square is a row and a column
type Square = (Integer, Integer)
-- A move is a relative offset to a square
type Move = (Integer, Integer)
-- A Board is a list of unoccupied squares
type Board = [Square]

-- A piece has a name, which is used to display a piece,
-- and a move predicate, which tells you if the piece can
-- perform that move.
type MovePredicate = Move -> Bool
data Piece = CreatePiece { name :: String, predicate :: MovePredicate}

instance Show Piece where
    show = name

instance Eq Piece where
    x == y = (name x) == (name y)

finiteMoves :: [Move] -> Move -> Bool
finiteMoves allowed move = elem move allowed

pawn = CreatePiece "P" $ finiteMoves [(1,-1), (1,1)]
rook = CreatePiece "R" $ \(r,c) -> r /= c && (r == 0 || c == 0)
knight = CreatePiece "N" $ \(r,c) -> elem (abs(r), abs(c)) [(1,2), (2,1)]
bishop = CreatePiece "B" $ \(r,c) -> r /= 0 && abs(r) == abs(c)
queen = CreatePiece "Q" $ \m -> (predicate bishop m) || (predicate rook m)
king = CreatePiece "K" $ finiteMoves [(r, c) | r <- [-1,0,1], c <- [-1,0,1], (r,c) /= (0,0)]

emptyBoard :: Integer -> Integer -> Board
emptyBoard rows columns =
    [(r,c) | r <- [1..rows], c <- [1..columns]]

standardBoard = emptyBoard 8 8

-- relativeMove finds out the move
-- required to get from square
-- x to square y
relativeMove :: Square -> Square -> Move
relativeMove (x2, y2) (x1, y1) = (x1-x2, y1-y2)

-- squares takes:
-- a Board, b
-- a Piece, p
-- a starting position, s
-- and returns the parts of the board
-- this piece can move to from
-- position s
squares :: Board -> Piece -> Square -> Board
squares b p s =
    let pred = (predicate p)
        canMove = \to -> pred (relativeMove s to) in
        filter canMove b

type Placement = (Square, Piece)
type Solution = [Placement]
solutions :: Board -> [Piece] -> [Solution]
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

