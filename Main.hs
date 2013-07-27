import ChessPuzzle
putLn :: (Show s) => s -> IO ()
putLn = putStrLn . show

main :: IO ()
main = do
    putLn (length test)
    return ()
