-- | Module Minimax
module Minimax (minimaxScore, getScoreFromPlayer) where

import State 
import Rules

-- | Retourne le score du minimax pour un état 'state', avec une profondeur
-- demandée de recherche 'depth'
minimaxScore :: State -> Int -> Int
minimaxScore state _ = getScoreFromBoard state 

-- | Permet d'obtenir le score du Jeu selon le joueur courant
getScoreFromBoard :: State -> Int
getScoreFromBoard state = getScoreFromPlayer (whoPlays state) state -
                             getScoreFromPlayer (switchPlayer (whoPlays state)) state

-- | Retourne le score du joueur courant
getScoreFromPlayer :: Player -> State -> Int
getScoreFromPlayer player gameState = do
    let listAxes = getAxesFromBoard gameState
    sum (mapGetScoreFromAxe player listAxes)

-- | Fonction utilisée par getScoreFromBoard pour mapper getScoreFromAxe
-- sur la liste des axes
mapGetScoreFromAxe :: Player -> [[Tile]] -> [Int]
mapGetScoreFromAxe player = map (getScoreFromAxe player)

-- | Retourne le score du joueur sur un axe -> 1 si alignement de 2 -> 10 si alignement de 3
getScoreFromAxe :: Player -> [Tile] -> Int
getScoreFromAxe player list = do
    let count = countAllConsecutivePieces player list
    if count == 1
        then 1
        else if count == 2
            then 10
            else 0










