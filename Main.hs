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
    putStrLn "Enter pieces"
    piecesString <- getLine
    let rows = read rowString
        columns = read columnString
        pieces = read piecesString
        solutions = chess rows columns pieces
        in
            if (atLeast 100 solutions) then
                do
                    putStrLn $ "Over 100 solutions. Computing..."
                    putStrLn $ "Total solutions: " ++ (show $ length solutions)
            else
                do
                    mapM_ print solutions
                    putStrLn $ "Total solutions: " ++ (show $ length solutions)
    return ()
