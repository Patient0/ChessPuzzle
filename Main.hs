import ChessPuzzle

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
            mapM_ print solutions
    return ()
