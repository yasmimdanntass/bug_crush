module Types where

-- O inseto (peça do jogo)
data Bug = Red | Blue | Green | Yellow | Purple | Empty
    deriving (Eq, Show)

-- O tabuleiro 
type Board = [[Bug]]

-- A coordenada da peça
type Coordinate = (Int, Int)

-- O estado do jogo 
data GameState = GameState {
    board :: Board,
    score :: Int,
    movesLeft :: Int
} deriving (Show)