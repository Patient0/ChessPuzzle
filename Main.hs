import ChessPuzzle

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast n (_:ys) = atLeast (n-1) ys

main :: IO ()
main = do
    putStrLn "Enter rows"
    rowString <- getLine
    putStrLn "Enter columns"
    columnString <- getLine
    putStrLn $ "Enter pieces. Any of ['" ++ pieceChars ++ "']"
    piecesString <- getLine
    let rows = read rowString
        columns = read columnString
        pieces = toPieces piecesString
        solutions = chess rows columns pieces
        in
            do
            if (atLeast 100 solutions) then
                putStrLn $ "Over 100 solutions..."
            else
                mapM_ print solutions
            putStrLn $ "Total solutions: " ++ (show $ length solutions)
    return ()
