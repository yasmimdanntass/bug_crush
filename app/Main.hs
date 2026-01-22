module Main where

import System.IO
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)

-- Importando nossos módulos
import Types
import Board (generateBoard)
import Render (printBoard)
import GameLogic (swap, findMatches, clearMatches, isValidMove, stepGravity, fixInitialMatches, points, detectGroups)
import UI (initialScreen, mainMenu, rulesScreen, instructionsScreen, loginScreen, renderHUD, clearScreen, gameOverScreen)

-- Loop de Animação de Queda
animateFall :: Board -> IO Board
animateFall board = do
    printBoard board
    threadDelay 100000 -- 0.1s
    (nextBoard, moved) <- stepGravity board
    if moved then animateFall nextBoard else return board

-- Loop de Resolução de Cascata
resolveCascades :: Board -> Int -> IO (Board, Int)
resolveCascades board comboMultiplier = do
    let groups = detectGroups board

    if null groups
        then return (board, 0)
        else do
            let -- 1. Pontuação base: soma simples dos grupos atuais
                basePts = sum (map points groups)
                
                -- 2. O bónus só existe se comboMultiplier > 0 (ou seja, se já houve uma queda)
                multiplier = if comboMultiplier == 0 
                                then 1.0 
                                else 1.0 + (0.5 * fromIntegral comboMultiplier)
                
                roundTotal = round (fromIntegral basePts * multiplier)
                
                -- Limpa todas as peças que deram match
                allCoords = concat groups
                cleared = clearMatches board allCoords

            printBoard cleared
            
            -- Feedback visual diferenciado
            if comboMultiplier == 0
                then putStrLn $ "\n   >>> EXPLOSÃO! +" ++ show roundTotal ++ " pts <<<"
                else putStrLn $ "\n   >>> CASCATA COMBO x" ++ show (comboMultiplier + 1) ++ "! +" ++ show roundTotal ++ " pts <<<"
            
            threadDelay 800000 
            
            -- 3. ANTES de aumentar o multiplier, as peças CAEM
            stableBoard <- animateFall cleared
            
            -- 4. Agora sim, chamamos recursivamente com multiplier + 1
            (finalBoard, cascadePoints) <- resolveCascades stableBoard (comboMultiplier + 1)
            
            return (finalBoard, roundTotal + cascadePoints)

-- Input do Usuário
getUserInput :: IO (Maybe (Coord, Coord))
getUserInput = do
    putStrLn "\nDigite: Linha Coluna Direção (w/s/a/d). 'q' para sair."
    putStr "> "
    hFlush stdout
    line <- getLine
    
    if line == "q" 
        then return Nothing 
        else case words line of
            -- Verifica se o usuário digitou exatamente 3 coisas
            [rStr, cStr, dir] -> 
                -- Tenta converter as duas primeiras strings para Int de forma segura
                case (readMaybe rStr, readMaybe cStr) of
                    (Just r1, Just c1) -> do
                        -- Se deu certo (são números), calcula o destino
                        let (r2, c2) = case dir of
                                "w" -> (r1-1, c1)
                                "s" -> (r1+1, c1)
                                "a" -> (r1, c1-1)
                                "d" -> (r1, c1+1)
                                _   -> (-1, -1) -- Direção inválida força erro
                        return $ Just ((r1, c1), (r2, c2))
                    
                    _ -> return $ Just ((-1,-1), (-1,-1)) -- Se não forem números, retorna inválido
            
            _ -> return $ Just ((-1,-1), (-1,-1)) -- Se digitou número errado de palavras

-- Loop Principal
--Precisei refatorar para ser possível exibir elementos da interface
--O loop principal do jogo agora recebe:
--  nome -> nome do jogador(vindo da tela de login)
--  pontos -> pontuação atual do jogador
--  movimentos -> movimentos restantes
--  board -> estado atual do tabuleiro
--A pontuação é mantida como parâmetro explícito com valores temporários
--aguardando a finalização da lógica de cálculo
-- Loop Principal do Jogo
gameLoop :: String -> Int -> Int -> Board -> IO () 
gameLoop name points movements board = do
    -- 1. Verifica Condição de Fim de Jogo
    if movements <= 0
        then gameOverScreen name points -- Exibe fim de jogo e RETORNA (sem chamar menu aqui)
        else do
            -- 2. Renderiza a Interface
            clearScreen
            printBoard board
            renderHUD name points movements
            
            -- 3. Pede Input
            input <- getUserInput
            case input of
                Nothing -> gameOverScreen name points -- Se digitou 'q', encerra
                Just (c1, c2) -> do
                    if not (isValidMove c1 c2) 
                        then do
                            putStrLn "\nMovimento inválido!"
                            threadDelay 1000000
                            gameLoop name points movements board
                        else do
                            -- 4. Executa a Troca
                            let swapped = swap board c1 c2
                            clearScreen
                            renderHUD name points movements
                            printBoard swapped
                            putStrLn "\n   >>> TROCANDO... <<<"
                            threadDelay 1000000 -- 1.0s
                    
                            -- 5. Verifica se houve Match
                            if null (findMatches swapped)
                                then do
                                    putStrLn "Sem combinação! Voltando..."
                                    threadDelay 1000000
                                    gameLoop name points movements board -- Desfaz o movimento (usa o board original)
                                else do
                                    -- 6. Resolve a Cascata (Explosões + Gravidade + Pontos)
                                    -- O '0' é o multiplier inicial do combo
                                    (finalBoard, gainedPoints) <- resolveCascades swapped 0
                                    
                                    let newPoints = points + gainedPoints
                                    let newMovements = movements - 1

                                    -- 7. Recursão: Chama o próximo turno
                                    gameLoop name newPoints newMovements finalBoard

startGame :: String -> IO ()
startGame name = do
    rawBoard <- generateBoard
    cleanBoard <- fixInitialMatches rawBoard
    
    let startPoints = 0
    let startMovements = 15
    
    -- Inicia o loop do jogo
    gameLoop name startPoints startMovements cleanBoard

-- Entrada do Programa
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initialScreen
    menuLoop

menuLoop :: IO ()
menuLoop = do
    option <- mainMenu
    case option of
        1 -> do
            name <- loginScreen
            startGame name
            menuLoop 
        2 -> rulesScreen >> menuLoop
        3 -> instructionsScreen >> menuLoop
        4 -> putStrLn "Saindo do jogo..." 
        _ -> menuLoop
