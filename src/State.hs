-- | Module State
module State (State(..), Player(..), Size(..), Piece, Sets(..), Tile(..), Board, Position(..), initialState) where

-- | Data représentant un joueur
data Player = Player | Minimax | Dummy deriving (Eq, Show)

instance Ord Player where
  compare Player Minimax = GT
  compare Player Dummy = GT
  compare Minimax Player = LT
  compare Minimax Dummy = GT
  compare Dummy Player = LT
  compare Dummy Minimax = LT
  compare _ _ = EQ

-- | Data représenatnt les tailles de pièces
data Size = T | S | M | B deriving (Eq, Show)
instance Ord Size where
  compare T T = EQ
  compare T _ = LT
  compare S T = GT
  compare S S = EQ
  compare S _ = LT
  compare M T = GT
  compare M S = GT
  compare M M = EQ
  compare M _ = LT
  compare B B = EQ
  compare B _ = GT

-- | Une pièce = (Joueur, taille)
type Piece = (Player, Size) 

-- | Data représentant les sets de pièces en jeu pour les deux joueurs
data Sets = Set { playerPieces :: [[Piece]], minimaxPieces :: [[Piece]] } deriving (Show)

-- | Tuile du tableau de jeu -> liste de pièce + position de la tuile sur le board
data Tile = Tile {pieces :: [Piece], position :: Position} deriving Show

-- | Data représentant une postion sur le board
data Position = Position {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

-- | Instance d'ordonnancement de Position (pour construction de Set)
instance Ord Position where
  compare (Position x1 y1) (Position x2 y2) =
    compare (x1, y1) (x2, y2)

-- | Board -> liste de liste de tuiles -> tableau de jeu
type Board = [[Tile]]

-- | Data représentant un état de jeu -> le tableau, les sets de pièces et le joueur en cours
data State = State {
    board :: Board,
    sets :: Sets,
    whoPlays :: Player   
} deriving (Show)


-- | État initial du jeu
initialState :: State
initialState = State {
    board = setInitBoard,
    sets = Set { 
                playerPieces = replicate 3 [(Player, B), (Player, M), (Player, S), (Player, T)],
                minimaxPieces = replicate 3 [(Minimax, B), (Minimax, M), (Minimax, S), (Minimax, T)] 
                },
    whoPlays = Player   
}

-- | Initialise le tableau de jeu avec une position pour chaque tuile vide
setInitBoard :: Board
setInitBoard = do
    x0 <- [0]
    y1 <- [0, 1, 2, 3]
    let tile0 = Tile [] (Position x0 y1)
    x1 <- [1]
    let tile1 = Tile [] (Position x1 y1)
    x2 <- [2]
    let tile2 = Tile [] (Position x2 y1)
    x3 <- [3]
    let tile3 = Tile [] (Position x3 y1)
    [[tile0, tile1, tile2, tile3]] 

















