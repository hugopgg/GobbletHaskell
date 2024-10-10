-- | Module Main
module Main (main, printGameState) where

import State
import Minimax
import Rules
import Move
import System.Console.ANSI
import Text.Printf
import System.IO (hFlush, stdout)
import System.Random (randomR, mkStdGen, StdGen)
import Data.Set (toList)


-- | Main
main :: IO ()
main = do
    hideCursor
    setTitle "GOBBLET"
    clearScreen
    setCursorPosition 0 0   
    printGameState initialState
    playTurn initialState

-- | Joue chaque tour des joueurs
playTurn :: State -> IO ()
playTurn gameState = do
  putStrLn $ "\nTon tour " ++ show (whoPlays gameState) ++ "!"
  putStrLn "Entrer votre mouvement:"
  readLine <- getLine
  let playerMove = readLine
  let maybeMove = parseMove playerMove  
  let gen = mkStdGen 37
  case maybeMove of
    Just move -> do        
      let maybeUpdatedState = applyMove gameState move      
      case maybeUpdatedState of
        Just updatedState -> do
          printGameState updatedState
          if checkWin (whoPlays gameState) updatedState
            then do
              putStrLn $ "\nBravo " ++ show (whoPlays gameState) ++ " à gagné!"
              playAgain <- askToPlayAgain
              if playAgain
                then main
                else putStrLn "\nAu revoir!"
          -- else playTurn updatedState   -- Jouer contre un humain
          else minimaxPlay gen updatedState >>= playTurn  -- Jouer contre 'ordi'/hasard
        Nothing -> do
          printGameState gameState
          putStrLn "\n*****Mouvement illégal!*****"
          playTurn gameState
    Nothing -> do
      printGameState gameState
      putStrLn "\n*****Syntaxe invalide!*****" 
      playTurn gameState 



-- | Selectionne un mouvement dans la liste des Move available selon l'état et le joue
minimaxPlay :: StdGen -> State -> IO State
minimaxPlay gen state = do
  let possibleMoves = toList (availableMoves state)
  let numMoves = length possibleMoves
  if numMoves == 0
    then return state
    else do
      let (randomIndex, _) = randomR (0, numMoves - 1) gen
          randomMove = possibleMoves !! randomIndex
          maybeUpdatedState = applyMove state randomMove
      case maybeUpdatedState of
        Just updatedState -> do
          printGameState updatedState
          return updatedState
        Nothing -> return state


-- | Affiche l'état de jeu
printGameState :: State -> IO ()
printGameState gameState = do
    clearScreen
    setCursorPosition 0 0
    hFlush stdout
    let gameBoard = State.board gameState
    let score = minimaxScore gameState 0 
    putStrLn "\nGOBBLET!\n\n"
    putStrLn  $ "Score : " ++ show score ++ "\n"
    putStrLn "+-----+-----+-----+-----+"
    printRow gameBoard 0
    putStrLn "+-----+-----+-----+-----+"
    printRow gameBoard 1
    putStrLn "+-----+-----+-----+-----+"
    printRow gameBoard 2
    putStrLn "+-----+-----+-----+-----+"
    printRow gameBoard 3
    putStrLn "+-----+-----+-----+-----+"

    putStrLn "\nPlayer Set: "
    printSets (playerPieces (sets gameState))
    putStrLn "Minimax Set: " 
    printSets (minimaxPieces (sets gameState))


-- | Affiche les lignes du board
printRow :: Board -> Int -> IO ()
printRow gameBoard row = do
    let tiles = gameBoard !! row
    printf "| %3s | %3s | %3s | %3s |\n"
        (printTile (head tiles))
        (printTile (tiles !! 1))
        (printTile (tiles !! 2))
        (printTile (tiles !! 3))


-- | Affiche tuile
printTile :: Tile -> String
printTile (Tile [] _) = " "
printTile (Tile piece _) = formatPiece (head piece) -- first piece
-- printTile (Tile pieces) = unwords (map formatPiece pieces)  -- liste des pieces (pour trace)


-- | Affiche les sets
printSets :: [[Piece]] -> IO ()
printSets = mapM_ printPieceSet

-- | Affiche un set
printPieceSet :: [Piece] -> IO ()
printPieceSet set = do
  putStrLn $ unwords (map formatPiece set)

-- | Format Piéce en string pour affichage
formatPiece :: Piece -> String
formatPiece (player, size) = showPlayer player ++ show size

-- | Affiche le joueur
showPlayer :: Player -> String
showPlayer Player = "P."
showPlayer Minimax = "M."
showPlayer Dummy = "D."

-- | Fonction appelée si partie terminée/gagnée -> Demande de rejouer
askToPlayAgain :: IO Bool
askToPlayAgain = do
    putStrLn "\nVoulez-vous jouer une autre partie? (oui/non)"
    choice <- getLine
    case choice of
        "oui" -> return True
        "non" -> return False
        _ -> do
            putStrLn "\n******Veuillez entrer 'oui' ou 'non'!******"
            askToPlayAgain














