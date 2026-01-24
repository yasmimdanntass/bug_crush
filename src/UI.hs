module UI (clearScreen, initialScreen, waitForM, mainMenu, loginScreen, rulesScreen, instructionsScreen, renderHUD, gameOverScreen) where

import System.IO (hFlush, stdout)

green, reset :: String
green = "\ESC[32m"
reset = "\ESC[0m"

-- Limpar a tela do terminal
clearScreen :: IO ()
clearScreen = do
    putStr "\ESC[2J"
    putStr "\ESC[H"

-- 1. Criação da Tela Inicial
initialScreen :: IO ()
initialScreen = do
    clearScreen
    putStrLn $ green ++ "                                            " ++ reset
    putStrLn $ green ++ "  ____  _    _  _____      _____ _____  _    _  _____ _    _ " ++ reset
    putStrLn $ green ++ " |  _ \\| |  | |/ ____|    / ____|  __ \\| |  | |/ ____| |  | |" ++ reset
    putStrLn $ green ++ " | |_) | |  | | |  __    | |    | |__) | |  | | (___ | |__| |" ++ reset
    putStrLn $ green ++ " |  _ <| |  | | | |_ |   | |    |  _  /| |  | |\\___ \\|  __  |" ++ reset
    putStrLn $ green ++ " | |_) | |__| | |__| |   | |____| | \\ \\| |__| |____) | |  | |" ++ reset
    putStrLn $ green ++ " |____/ \\____/ \\_____|    \\_____|_|  \\_\\\\____/|_____/|_|  |_|" ++ reset
    putStrLn ""
    --putStrLn $ green ++ "     ~ 🐜 ~ 🐞 ~ 🐝 ~ 🦗 ~ 🕸️ ~" ++ reset
    putStrLn $ green ++ " [ Pressione a tecla 'M' para ir ao Menu Inicial ]" ++ reset
    waitForM

--2. Esperar o usuário digitar a entrada válida
waitForM :: IO ()
waitForM = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == "m" || input == "M"
        then return ()
        else do
            putStrLn "Entrada inválida. Aperte 'M' para continuar."
            waitForM

--3. Criação do Menu Inicial
mainMenu :: IO Int
mainMenu = do
    clearScreen
    putStrLn "===================="
    putStrLn "      BUG CRUSH     "
    putStrLn "===================="
    putStrLn "1-Iniciar Jogo"
    putStrLn "2-Regras"
    putStrLn "3-Instruções" 
    putStrLn "4-Sair"
    putStrLn "Escolha uma opção: "
    hFlush stdout
    
    input <- getLine
    case reads input :: [(Int, String)] of
        [(opcao, "")] -> return opcao
        _ -> do
            putStrLn "Opção inválida. Pressione ENTER e tente novamente."
            _ <- getLine
            mainMenu

--4. Criação da tela de login acessada antes de iniciar o jogo
loginScreen :: IO String 
loginScreen = do 
    clearScreen
    putStrLn "===== LOGIN ====="
    putStrLn ""
    putStr "Digite o seu nome: "
    hFlush stdout
    name <- getLine
    putStrLn ""
    putStrLn ("Bem vindo(a), " ++ name ++ "!")
    putStrLn "Pressione ENTER para iniciar o jogo..."
    _ <- getLine
    return name

--5. Exibe a tela com as regras do jogo
rulesScreen :: IO ()
rulesScreen = do
    clearScreen
    putStrLn "===== REGRAS ===== "
    putStrLn "1-Troque duas peças vizinhas na horizontal ou na vertical."
    putStrLn "2-Forme combinações de 3 ou mais peças iguais."
    putStrLn "3-Cada troca consome um movimento."
    putStrLn "4-A fase termina quando os movimentos acabam ou quando o jogador decide sair."
    putStrLn "5-O jogador vence quando atinge 500 pontos."
    putStrLn ""
    putStrLn "Pressione [ENTER] para retornar ao Menu Inicial"
    _ <- getLine
    return ()   

--5. Exibe as instruções de como o jogador pode realizar uma combinação
instructionsScreen :: IO ()
instructionsScreen = do
    clearScreen
    putStrLn "===== INSTRUÇÕES ====="
    putStrLn "Digite as coordenadas das peças e a direção do movimento para trocar suas posições."
    putStrLn ""
    putStrLn "Os comandos para as direções funcionam da seguinte maneira:"
    putStrLn " - w: mover a peça para cima"
    putStrLn " - a: mover a peça para a esquerda"
    putStrLn " - s: mover a peça para baixo"
    putStrLn " - d: mover a peça para a direita"
    putStrLn ""
    putStrLn "Formato: linha coluna direção"
    putStrLn "Exemplo: 1 2 w"
    putStrLn ""
    putStrLn "Pressione [ENTER] para retornar ao Menu Inicial"
    _ <- getLine
    return ()

-- Define a largura interna da caixa (sem contar as bordas laterais)
boxWidth :: Int
boxWidth = 40 

-- Função auxiliar para criar uma linha formatada
-- Ela pega o label ("Pontos: ") e o valor ("100") e preenche o meio com espaços
formatLine :: String -> String -> String
formatLine label value = 
    let contentLen = length label + length value
        paddingLen = boxWidth - contentLen
        padding    = replicate paddingLen ' ' -- Cria os espaços que faltam
    in "║ " ++ label ++ value ++ padding ++ " ║"

--6. Renderiza a HUD (informações do jogador durante o jogo)
--Neste momento a HUD já suporta exibição de pontos e movimentos
--restantes, mesmo que a lógica da atualização de pontos e movimentos
--ainda esteja sendo implementada no módulo de lógica
renderHUD :: String -> Int -> Int -> IO ()
renderHUD name points movements = do
    putStrLn ""
    putStrLn ""
    putStrLn $ "╔" ++ replicate (boxWidth + 2) '═' ++ "╗"
    putStrLn $ formatLine "Jogador: " name
    putStrLn $ formatLine "Pontos:  " (show points)
    putStrLn $ formatLine "Movimentos Restantes: " (show movements) 
    putStrLn $ "╚" ++ replicate (boxWidth + 2) '═' ++ "╝"
    putStrLn ""

--7. Tela de fim de jogo
gameOverScreen :: String -> Int -> IO ()
gameOverScreen name points = do
    clearScreen
    putStrLn "╔═══════════════════════════════════╗"
    putStrLn "║             FIM DE JOGO           ║"
    putStrLn "╚═══════════════════════════════════╝"
    putStrLn (" Jogador: " ++ name)
    putStrLn (" Pontuação final: " ++ show points)
    putStrLn ""
    
    -- Lógica de vitória ou de derrota
    if points >= 500
        then putStrLn "      PARABÉNS! VOCÊ VENCEU!    "
        else putStrLn "      QUE PENA! VOCÊ PERDEU.    "
        
    putStrLn ""
    putStrLn " Pressione [ENTER] para voltar ao menu"
    _ <- getLine
    return ()
