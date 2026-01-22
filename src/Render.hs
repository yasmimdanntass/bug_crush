module Render where

import Types

-- Cores ANSI
colorStr :: Bug -> String
colorStr Red    = "\ESC[31mO\ESC[0m"
colorStr Blue   = "\ESC[34mO\ESC[0m"
colorStr Green  = "\ESC[32mO\ESC[0m"
colorStr Yellow = "\ESC[33mO\ESC[0m"
colorStr Purple = "\ESC[35mO\ESC[0m"
colorStr Empty  = " "

-- Limpa a tela e imprime tudo
printBoard :: Board -> IO ()
printBoard board = do
    putStr "\ESC[2J\ESC[H" -- Limpa tela
    putStrLn "   0 1 2 3 4 5 6 7"
    putStrLn "   ----------------"
    printRows board 0
    putStrLn "   ----------------"

printRows :: Board -> Int -> IO ()
printRows [] _ = return ()
printRows (row:rows) i = do
    putStr (show i ++ " |")
    mapM_ (\c -> putStr (colorStr c ++ " ")) row
    putStrLn "|"
    printRows rows (i + 1)