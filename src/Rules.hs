{-# LANGUAGE LambdaCase #-}
-- | Module Rules
module Rules (availableMoves, applyMoves, applyMove, switchPlayer, countAllConsecutivePieces, getAxesFromBoard, checkWin, getTopPiecesFromBoardForPlayer,getTopPiecesFromBoard) where
    
import Move 
import State 
import qualified Data.Set as S
import Control.Monad (foldM)


-- | Essaye d'appliquer plusieurs mouvements consécutifs à partir de l'état initial du jeu.
applyMoves :: [Move] -> Maybe State
applyMoves = foldM applyMove initialState


-- | Essaye d'appliquer un mouvement selon les règlements
-- Si mouvement appliqué -> mise à jours du set du joueur
applyMove :: State -> Move -> Maybe State
applyMove state move = case move of
    DropMove piece pos -> do
        let currentPlayer = whoPlays state
        let size = snd piece
        let newPiece = (currentPlayer, size)
        updateBoard <- dropPiece newPiece pos (board state)
        let updatedSets = case newPiece of
                (Player, _) ->
                        if isFirstPiece newPiece (playerPieces (sets state))
                            then Just $ (sets state) { playerPieces = removePieceFromSets newPiece (playerPieces (sets state)) }
                            else Nothing 
                (Minimax, _) ->
                        if isFirstPiece newPiece (minimaxPieces (sets state))
                            then Just $ (sets state) { minimaxPieces = removePieceFromSets newPiece (minimaxPieces (sets state)) }
                            else Nothing 
                (Dummy, _) -> Nothing
        updatedSets' <- updatedSets  
        let updatedState = state { board = updateBoard, sets = updatedSets', whoPlays = switchPlayer currentPlayer }                  
        return updatedState

    OnboardMove fromPos toPos -> do
        let currentPlayer = whoPlays state
        updateBoard <- movePiece currentPlayer fromPos toPos (board state)
        return $ state { board = updateBoard, whoPlays = switchPlayer currentPlayer }


-- | Application du dropMove
dropPiece :: Piece -> Position -> Board -> Maybe Board
dropPiece piece pos b = do
    let tile = getTile pos b
    updatedTile <- addPiece piece tile
    return $ setTile pos updatedTile b


-- | Application du onboardMove
movePiece :: Player -> Position -> Position -> Board -> Maybe Board     
movePiece player fromPos toPos b = do
    let fromTile = getTile fromPos b
    let whosePiece = case pieces fromTile of
                    (piece:_) -> Just (fst piece)  
                    _ -> Nothing                                         
    case whosePiece of
        Just piecePlayer | piecePlayer == player -> do
            let toTile = getTile toPos b
            (piece, updatedFromTile) <- removePiece fromTile
            updatedToTile <- addPiece piece toTile
            let updatedBoard = setTile fromPos updatedFromTile $ setTile toPos updatedToTile b
            return updatedBoard
        _ -> Nothing


-- | Retourne la tuile selon une position et un board
getTile :: Position -> Board -> Tile
getTile (Position x1 y1) b = b !! y1 !! x1


-- | Modifie la tuile à postition sur board et retourne le nouveau board
setTile :: Position -> Tile -> Board -> Board
setTile (Position x1 y1) tile b = replace y1 (replace x1 tile (b !! y1)) b
    where
        replace :: Int -> a -> [a] -> [a]
        replace index newValue list = take index list ++ [newValue] ++ drop (index + 1) list

-- | Ajoute pièce sur tuile
addPiece :: Piece -> Tile -> Maybe Tile
addPiece piece tile = case pieces tile of
  [] -> Just $ tile { pieces = [piece] }
  (existingPiece:_) -> if size piece > size existingPiece && size piece /= size existingPiece
    then Just $ tile { pieces = piece : pieces tile }
    else Nothing
  where
    size (_, s) = s


-- | Enlève pièce de tuile
removePiece :: Tile -> Maybe (Piece, Tile)
removePiece tile = case pieces tile of
    [] -> Nothing
    (piece:rest) -> Just (piece, tile { pieces = rest})

-- | Enlève pièce des sets
removePieceFromSets :: Piece -> [[Piece]] -> [[Piece]]
removePieceFromSets = removeFirstPiece

-- | Enlève la première pièce du set
removeFirstPiece :: Piece -> [[Piece]] -> [[Piece]]
removeFirstPiece _ [] = []
removeFirstPiece piece (x1:xs)
  | piece `elem` x1 = removePieceFromSet piece x1 : xs
  | otherwise = x1 : removeFirstPiece piece xs

-- | Enlève pièce d'un set
removePieceFromSet :: Piece -> [Piece] -> [Piece]
removePieceFromSet _ [] = []
removePieceFromSet piece (x1:xs)
  | piece == x1 = xs
  | otherwise = x1 : removePieceFromSet piece xs

-- | Retourne True si pièce est premier élément 
isFirstPiece :: Piece -> [[Piece]] -> Bool
isFirstPiece piece = any $ \case
    (x1:_) -> x1 == piece
    _ -> False

-- | Change le joueur en cours
switchPlayer :: Player -> Player
switchPlayer player = case player of
    Player -> Minimax
    Minimax -> Player
    Dummy -> Dummy

-- =============================================================================
-- *************** Utils pour get des informations sur un board ****************
-- =============================================================================

-- | Retourne True si un joueur à un aligement de 4 sur un axe
checkWin :: Player -> State ->  Bool
checkWin player gameState = do
    let listAxes = getAxesFromBoard gameState
    let heads = concatMap (searchWinFromAxe player) listAxes
    not (null heads)


-- | Retourne une liste des tetes d'un axe si alignement de 4 
searchWinFromAxe :: Player -> [Tile] -> [TopPiece]
searchWinFromAxe player list
    | length tetes == 4 && all (\t -> getPlayerTopPiece t == player) tetes = tetes
    | otherwise = []
    where tetes = getTopPiecesFromAxe list


-- | Retourne l'occurence consécutive d'un joueur sur un axe (même les pièces recouvertes)
countAllConsecutivePieces :: Player -> [Tile] -> Int
countAllConsecutivePieces _ [] = 0
countAllConsecutivePieces _ [_] = 0
countAllConsecutivePieces player (x1:y1:xs) =
    if player `elem` getPlayersFromTile x1 && player `elem` getPlayersFromTile y1
        then 1 + countAllConsecutivePieces player (y1:xs)
        else countAllConsecutivePieces player (y1:xs)


-- | Retourne la liste des joueurs présents sur un tuile sans doublons.
getPlayersFromTile :: Tile -> [Player]
getPlayersFromTile tile = do
    let liste = tile2Liste tile
    uniqPlayer (getPlayersFromList liste)

-- | Transforme une tuile en liste de pièce
tile2Liste :: Tile -> [Piece]
tile2Liste = pieces

-- | Retire les doublons d'une liste de joueurs
uniqPlayer :: [Player] -> [Player]
uniqPlayer [] = []
uniqPlayer (x1:xs) = x1 : uniqPlayer (filter (/=x1) xs)

-- | Retourne la liste des joueurs d'une liste de pieces.
getPlayersFromList :: [Piece] -> [Player]
getPlayersFromList [] = []
getPlayersFromList [x1] = [fst x1]
getPlayersFromList (x1:xs) = fst x1 : getPlayersFromList xs


-- | Retourne une liste des axes
getAxesFromBoard :: State -> [[Tile]]
getAxesFromBoard gameState = [
    getRow (board gameState) 0,
    getRow (board gameState) 1,
    getRow (board gameState) 2,
    getRow (board gameState) 3,
    getColumn (board gameState) 0,
    getColumn (board gameState) 1,
    getColumn (board gameState) 2,
    getColumn (board gameState) 3,
    getDiagLeft (board gameState),
    getDiagRight (board gameState)
    ]

-- | Retourne les lignes d'un tableau de jeu
getRow :: Board -> Int -> [Tile]
getRow b r = b !! r

-- | Retourne les colonnes d'un tableau de jeu
getColumn :: Board -> Int -> [Tile]
getColumn b c = 
    [head b !! c,
    (b !! 1) !! c,
    (b !! 2) !! c,
    (b !! 3) !! c]

-- | Retourne la diagonale gauche-droite d'un tableau de jeu
getDiagLeft :: Board -> [Tile]
getDiagLeft b =
    [head (head b),
    (b !! 1) !! 1,
    (b !! 2) !! 2,
    (b !! 3) !! 3]


-- | Retourne la diagonale droite-gauche d'un tableau de jeu
getDiagRight :: Board -> [Tile]
getDiagRight b = 
    [head b !! 3,
    (b !! 1) !! 2,
    (b !! 2) !! 1,
    head (b !! 3)]

-- | Retourne une liste des positions des cases vides du tableau
getFreeTileFromBoard :: State -> [Position]
getFreeTileFromBoard gameState = 
    getFreeTileFromAxe (getRow (board gameState) 0) ++
    getFreeTileFromAxe (getRow (board gameState) 1) ++
    getFreeTileFromAxe (getRow (board gameState) 2) ++
    getFreeTileFromAxe (getRow (board gameState) 3)
    
-- | Retourne une liste des positions des cases vides d'un axe
getFreeTileFromAxe :: [Tile] -> [Position]
getFreeTileFromAxe [] = []
getFreeTileFromAxe [x1] = checkFreeTile x1
getFreeTileFromAxe (x1:xs) = checkFreeTile x1 ++ getFreeTileFromAxe xs

-- | Retourne position si case vide
checkFreeTile :: Tile -> [Position]
checkFreeTile tile = do
    case pieces tile of
        [] -> [position tile]
        _  -> []

-- Retourne une liste des pièces de tête de pile du joueur
-- (Les goblets du dessus de ses trois piles)
getSetHeads :: Player -> State -> [Piece]
getSetHeads player gameState = do
    if player == Player
        then do
            let set = playerPieces (sets gameState)
            uniqPiece (getStackHead (head set) ++ getStackHead (set !! 1) ++ getStackHead (set !! 2))
        else do
            let set = minimaxPieces (sets gameState)
            uniqPiece( getStackHead (head set) ++ getStackHead (set !! 1) ++ getStackHead (set !! 2))
                where
                    getStackHead :: [Piece] -> [Piece]
                    getStackHead stack = [head stack | not (null stack)]
                    uniqPiece :: [Piece] -> [Piece]
                    uniqPiece [] = []
                    uniqPiece (x1:xs) = x1 : uniqPiece (filter (/=x1) xs)


-- | Retourne une liste des pièces du dessus du tableau
getTopPiecesFromBoard :: State -> [TopPiece]
getTopPiecesFromBoard gameState =
    getTopPiecesFromAxe (getRow (board gameState) 0) ++
    getTopPiecesFromAxe (getRow (board gameState) 1) ++
    getTopPiecesFromAxe (getRow (board gameState) 2) ++
    getTopPiecesFromAxe (getRow (board gameState) 3)

-- | Retourne une liste des pièce du dessus de Player
getTopPiecesFromBoardForPlayer :: Player -> [TopPiece] -> [TopPiece]
getTopPiecesFromBoardForPlayer _ [] = []
getTopPiecesFromBoardForPlayer player (x1:xs) =
    if player == getPlayerTopPiece x1
        then x1 : getTopPiecesFromBoardForPlayer player xs
        else getTopPiecesFromBoardForPlayer player xs


-- | Retourne une liste des pièces du dessus d'un axe
getTopPiecesFromAxe :: [Tile] -> [TopPiece]
getTopPiecesFromAxe [] = []
getTopPiecesFromAxe [x1] = getTopPieceTile x1
getTopPiecesFromAxe (x1:xs) = getTopPieceTile x1 ++ getTopPiecesFromAxe xs
    
-- | Retourne pièce du dessus d'une case
getTopPieceTile :: Tile -> [TopPiece]
getTopPieceTile tile = do
    case pieces tile of
        [] -> []
        _ -> [(head (pieces tile), position tile)]

-- | Retourne une liste de pièce du dessus si alignement de 3
searchAlign3FromAxe :: Player -> [Tile] -> [TopPiece]
searchAlign3FromAxe player list = do
  let opponent = switchPlayer player 
  let heads = getTopPiecesFromAxe list
  let alignedPieces = filter (\piece -> getPlayerTopPiece piece == opponent) heads
  if length alignedPieces >= 3
    then take 3 alignedPieces
    else []


-- | Retourne le joueur correspondant à une pièce du dessus
getPlayerTopPiece :: TopPiece -> Player
getPlayerTopPiece tp = fst (fst tp)

-- | Enlève les doublons pour ne pas créer des moves en double
uniqTopPiece :: [TopPiece] -> [TopPiece]
uniqTopPiece [] = []
uniqTopPiece (x1:xs) = x1 : uniqTopPiece (filter (/=x1) xs)


-- ****************************** Fin utils ************************************
-- =============================================================================
-- *************************** Moves disponibles *******************************


-- | Type utilisé pour la recherche des coups disponibles (pièce du dessus)
type TopPiece = (Piece, Position)

-- | Retourne l'ensemble des mouvements possibles à partir d'un état de jeu.
availableMoves :: State -> S.Set Move
availableMoves state =
  let currentPlayer = whoPlays state
      moves = getDropMovesEmptyTile state currentPlayer
              ++ getDropMovesOver3 state currentPlayer
              ++ getOnboardEmpty state currentPlayer
              ++ getOnboardMine state currentPlayer
              ++ getOnboardOpponent state currentPlayer
  in if checkWin (switchPlayer currentPlayer) state || checkWin currentPlayer state
       then S.empty
       else S.fromList moves


-- ****DROPS

-- | Retourne une liste de Move correspondant aux DropMove possible sur cases vides
getDropMovesEmptyTile :: State -> Player -> [Move]
getDropMovesEmptyTile state player = do
    let positions = getFreeTileFromBoard state
    let heads = getSetHeads player state
    createDropEmpty heads positions

-- | Retourne une liste de Move correspondant aux DropMove possible sur une case
-- occupée (alignement de 3 de l'adversaire)
getDropMovesOver3 :: State -> Player -> [Move]
getDropMovesOver3 state player = do
    let axes = getAxesFromBoard state
    let topPieces = concatMap (searchAlign3FromAxe player) axes
    let topPiecesUniq = uniqTopPiece topPieces
    let p = getSetHeads player state
    let maybeMoves = createDropOver p topPiecesUniq
    maybe2Move maybeMoves

-- | Crée les dropMoves pour chaque tête de pile et pour 
-- chaque position libre (tuile qui ne sont pas occupées)
createDropEmpty :: [Piece] -> [Position] -> [Move]
createDropEmpty xs ys = concatMap (\ x1 -> map (buildDrop x1) ys) xs
    where
        buildDrop :: Piece -> Position -> Move
        buildDrop piece pos = Move.DropMove piece pos


-- | crée les dropMoves respectant la règle de l'alignement de 3 de l'adversaire
createDropOver :: [Piece] -> [TopPiece] -> [Maybe Move]
createDropOver xs ys = concatMap (\ x1 -> map (buildDrop x1) ys) xs
    where        
        buildDrop :: Piece -> TopPiece -> Maybe Move
        buildDrop piece h = do
            if snd piece > snd (fst h)  
                then Just (Move.DropMove piece (snd h))
                else Nothing

-- | Conversion de maybe move à move
maybe2Move :: [Maybe Move] -> [Move]
maybe2Move [] = []
maybe2Move [x1] = do
    case x1 of
        Just move -> [move]
        Nothing -> []
maybe2Move (x1:xs) = do
    case x1 of
        Just move -> move : maybe2Move xs
        Nothing -> maybe2Move xs

-- ****ONBOARDS

-- | Retourne une liste de Move correspondant au OnboardMove disponible sur l'adversaire
getOnboardOpponent :: State -> Player -> [Move]
getOnboardOpponent state player = do
    let listAllHeads = getTopPiecesFromBoard state
    let playerHeads = getTopPiecesFromBoardForPlayer player listAllHeads
    let opponentsHeads = getTopPiecesFromBoardForPlayer (switchPlayer player) listAllHeads
    let maybeMoves = createOnboardToTopPiece playerHeads opponentsHeads
    maybe2Move maybeMoves

-- | Retourne une liste de Move correspondant au OnboardMove disponible sur ses pièces
getOnboardMine :: State -> Player -> [Move]
getOnboardMine state player = do
    let listAllHeads = getTopPiecesFromBoard state
    let playerHeads = getTopPiecesFromBoardForPlayer player listAllHeads
    let maybeMoves = createOnboardToTopPiece playerHeads playerHeads
    maybe2Move maybeMoves

-- | Retourne une liste de Move correspondant au OnboardMove disponible sur cases vides
getOnboardEmpty :: State -> Player -> [Move]
getOnboardEmpty state player = do
    let listAllHeads = getTopPiecesFromBoard state
    let playerHeads = getTopPiecesFromBoardForPlayer player listAllHeads
    let emptyTiles = getFreeTileFromBoard state
    let maybeMoves = createOnboardToPosition playerHeads emptyTiles
    maybe2Move maybeMoves

-- | Crée les OnboardMove sur les cases occupant une pièce
createOnboardToTopPiece :: [TopPiece] -> [TopPiece] -> [Maybe Move]
createOnboardToTopPiece xs ys = concatMap (\ x1 -> map (buildOnBoard x1) ys) xs
    where
        buildOnBoard :: TopPiece -> TopPiece -> Maybe Move
        buildOnBoard headPlayer headOpponent = do
            if snd (fst headPlayer) > snd (fst headOpponent)   
                then Just (Move.OnboardMove (snd headPlayer) (snd headOpponent))
                else Nothing

-- | Crée les OnboardMove sur les cases vides
createOnboardToPosition :: [TopPiece] -> [Position] -> [Maybe Move]
createOnboardToPosition xs ys = concatMap (\ x1 -> map (buildOnBoard x1) ys) xs
    where   
        buildOnBoard :: TopPiece -> Position -> Maybe Move
        buildOnBoard tp pos = Just (Move.OnboardMove (snd tp) pos)













