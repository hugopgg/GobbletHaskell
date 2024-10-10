-- | Module Move
module Move (parseMoves, parseMove, isValidPosition, Move(..)) where

import State
import Text.ParserCombinators.Parsec
import Data.Maybe (mapMaybe)


-- | Data représentant un move, soit un move à partir d'une pièce dans le set du joueur
-- ou à partir du board
data Move = DropMove Piece Position
          | OnboardMove Position Position
          deriving (Show)

-- | Instance d'égalité de Move (pour construction de Set)
instance Eq Move where
  (==) (DropMove piece1 pos1) (DropMove piece2 pos2) =
    piece1 == piece2 && pos1 == pos2
  (==) (OnboardMove from1 to1) (OnboardMove from2 to2) =
    from1 == from2 && to1 == to2
  (==) _ _ = False

-- | Instance d'ordonnancement de Move (pour construction de Set)
instance Ord Move where
  compare (DropMove piece1 pos1) (DropMove piece2 pos2) =
    compare (piece1, pos1) (piece2, pos2)
  compare (DropMove _ _) (OnboardMove _ _) = GT
  compare (OnboardMove _ _) (DropMove _ _) = LT
  compare (OnboardMove from1 to1) (OnboardMove from2 to2) =
    compare (from1, to1) (from2, to2)


  -- ===========================================================================

-- | Lit une liste de mouvement depuis une liste.
-- Chaque élément de la liste représente un unique mouvement.
-- On fait l'hypothèse que les mouvements sont correctement représentés.
-- Si ce n'est pas le cas, cette fonction peut émettre une exception qui arrête le programme.
parseMoves :: [String] -> [Move]
parseMoves = mapMaybe parseMove

-- | Parse un string en move
parseMove :: String -> Maybe Move
parseMove input = case parse moveParser "" (removeChar ' ' input) of
    Left _ -> Nothing
    Right move -> Just move

-- | Parse soit un dropMove ou un onboardMove
moveParser :: Parser Move
moveParser = try dropMoveParser <|> onboardMoveParser 

-- | Parse dropMove -> drop(taille, (position))
dropMoveParser :: Parser Move
dropMoveParser = do
    _ <- string "drop("
    size <- sizeParser
    _ <- string ","
    p <- positionParser
    _ <- char ')'      
    let piece = (Dummy, size)  
    return $ DropMove piece p

-- | Parse onboardMove -> onboard((position), (position))
onboardMoveParser :: Parser Move
onboardMoveParser = do
    _ <- string "onboard("
    fromPosition <- positionParser
    _ <- string ","
    toPosition <- positionParser
    _ <- char ')'
    return $ OnboardMove fromPosition toPosition

-- | Parse la position -> (x,y)
positionParser :: Parser Position
positionParser = do
    _ <- char '('
    posx <- intParser
    _ <- char ','
    posy <- intParser
    _ <- char ')'
    if isValidPosition posx posy
      then return $ Position posx posy
      else fail "Invalid position"

-- | Parse int
intParser :: Parser Int
intParser = read <$> many1 digit

-- | Parse la taille de pièce -> T | S | M | B
sizeParser :: Parser Size
sizeParser = do
  s <- oneOf "TSMB"
  case s of
    'T' -> return T
    'S' -> return S
    'M' -> return M
    'B' -> return B
    _ -> fail "Invalid size"

-- | Valide la position sur le board (4x4)
isValidPosition :: Int -> Int -> Bool
isValidPosition px py = px >= 0 && px <= 3 && py >= 0 && py <= 3

-- | Enlève un caractère d'un string (pour enlever les espaces avant le parsing)
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar ch (c:cs)
    | c == ch =  removeChar ch cs
    | otherwise = c : removeChar ch cs



