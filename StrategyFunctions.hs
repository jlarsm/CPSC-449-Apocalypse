{-|
Module      :   StrategyFunctions
Description :   All the commonly shared functions used between current and possibly future
                strategies. Function names may need to be revised to prevent overloaded function names,
                similar names exist between this module and the strategies.
Stability   :   experimental
Portability :   ghc 7.10.2 - 7.10.3
-}
module StrategyFunctions where

import System.IO.Unsafe
import System.Random
import ApocTools

-- |Returns a list of 2-tuples that contain the coordinates of all pieces of specified piece type
coordsOfPieceType :: Board -> Player -> PieceType -> [(Int, Int)]
coordsOfPieceType board playerType pType
    | (playerType == White && pType == Knight) = coordinatesOf board WK 0 0 0 0 4 4
    | (playerType == White && pType == Pawn)   = coordinatesOf board WP 0 0 0 0 4 4
    | (playerType == Black && pType == Knight) = coordinatesOf board BK 0 0 0 0 4 4
    | (playerType == Black && pType == Pawn)   = coordinatesOf board BP 0 0 0 0 4 4

-- Returns a list of all 2-tuple coordinates that contain specifiedPiece
coordinatesOf :: Board -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
coordinatesOf board specifiedPiece x y xMin yMin xMax yMax 
    | (x == xMax && y == yMax) =
        if (getFromBoard board (xMax, yMax)) == specifiedPiece 
        then (x, y) : []
        else []
    | (x == xMax) = 
        if (getFromBoard board (xMax, y)) == specifiedPiece
        then (xMax, y) : coordinatesOf board specifiedPiece xMin (y + 1) xMin yMin xMax yMax
        else coordinatesOf board specifiedPiece xMin (y + 1) xMin yMin xMax yMax
    | otherwise = 
        if (getFromBoard board (x, y)) == specifiedPiece
        then (x, y) : coordinatesOf board specifiedPiece (x + 1) y xMin yMin xMax yMax
        else coordinatesOf board specifiedPiece (x + 1) y xMin yMin xMax yMax

-- | Pseudo random 50pct chance to return true and 50pct to return false
coinFlip :: Bool
coinFlip =  (rand <= 0.5)
    where rand = unsafePerformIO (randomIO :: IO Float)

-- |Randomly picks an element from any given list
pickFromList :: [a] -> Maybe a
pickFromList [] = Nothing
pickFromList xs = Just (xs !! index)
    where index = floor ((unsafePerformIO (randomIO :: IO Float)) * (fromIntegral (length xs)))
                  
-- |Randomly picks an element from a list, higher chance towards picking the earlier indices
randomlyPickFromList :: [a] -> Float -> Maybe a
randomlyPickFromList [] chance= Nothing
randomlyPickFromList (x:[]) chance = Just x
randomlyPickFromList (x:xs) chance
    | rand <= chance = Just x
    | otherwise = randomlyPickFromList xs chance
    where rand = unsafePerformIO (randomIO :: IO Float)

-- |Lists out all the valid moves that doesn't take an enemy piece
listRegularMoves :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
listRegularMoves board move coord 0 = []
listRegularMoves board (move:restOfMoves) coord index 
    | isRegular board coord to = to : (listRegularMoves board restOfMoves coord (index - 1))
    | otherwise = listRegularMoves board restOfMoves coord (index - 1)
    where to = (fst coord + fst move, snd coord + snd move)

-- |Determines whether a move is a regular move, that it is not for moving to safety nor attacking
isRegular :: Board -> (Int, Int) -> (Int, Int) -> Bool
isRegular board from to | (checkBounds from && checkBounds to && getFromBoard board to == E) = True
                    | otherwise = False
                    
-- |Returns a list of 2-tuple regular moves that do not end up attacking an enemy piece
allRegulars :: Board -> (Int, Int) -> [(Int, Int)]
allRegulars board coord 
    | (piece == Knight) = listRegularMoves board knightMoves coord (length knightMoves)
    | (piece == Pawn)   = listRegularMoves board pawnMoves coord (length pawnMoves)
    where   piece = typeOf (pieceOf (getFromBoard board coord))
            player = playerOf (pieceOf (getFromBoard board coord))
            knightMoves = getKnightMoves
            pawnMoves = getPawnMoves player False

-- |Returns whether a position is currently safe from attacks for a player
isSafe :: Board -> Player -> (Int, Int) -> Bool
isSafe board player coord = isPositionSafeFromAttacks (allPlayerMoves board (getOpponent player)) coord

-- |Returns true if a position on the board is safe from attacks
isPositionSafeFromAttacks :: [(Int, Int)] -> (Int, Int) -> Bool
isPositionSafeFromAttacks [] coord = True
isPositionSafeFromAttacks (move:restOfMoves) coord
    | (move == coord) = False
    | otherwise = isPositionSafeFromAttacks restOfMoves coord

-- |Returns a list of valid "kill" moves that a piece can make
attacks :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
attacks board move coord 0 = []
attacks board (move:restOfMoves) coord index
    | isAttack board coord to = to : (attacks board restOfMoves coord (index - 1))
    | otherwise = attacks board restOfMoves coord (index - 1)
    where to = (fst coord + fst move, snd coord + snd move)

-- |Returns true if piece is attacking, False otherwise
isAttack :: Board -> (Int, Int) -> (Int, Int) -> Bool
isAttack board from to = (checkBounds from && checkBounds to && getFromBoard board to /= E &&
                   (playerOf (pieceOf (getFromBoard board from))) /= (playerOf (pieceOf (getFromBoard board to))))

-- |Returns a list of all the attacks that a single piece can make
allAttacks :: Board -> (Int, Int) -> [(Int, Int)]
allAttacks board coord  
    | (piece == Knight) = attacks board knightMoves coord (length knightMoves)
    | (piece == Pawn)   = attacks board pawnMoves coord (length pawnMoves)
    | otherwise = []
    where   piece = typeOf (pieceOf (getFromBoard board coord))
            player = playerOf (pieceOf (getFromBoard board coord))
            knightMoves = getKnightMoves
            pawnMoves = getPawnMoves player True

-- |Returns all the moves a player can make
allPlayerMoves :: Board -> Player -> [(Int, Int)]
allPlayerMoves board player = allMoves board (coordsOfPieceType board player Knight) ++ allMoves board (coordsOfPieceType board player Pawn)

-- |Returns a list of moves from a list of positions
allMoves :: Board -> [(Int, Int)] -> [(Int, Int)]
allMoves board [] = []
allMoves board (coord:coords) = getMoves board coord False ++ allMoves board coords

-- |Returns all the moves from a piece and whether it has to attack
getMoves :: Board -> (Int, Int) -> Bool -> [(Int, Int)]
getMoves board coord valid
    | (piece == Knight) = getAllMoves board knightMoves coord (length knightMoves) valid
    | (piece == Pawn)   = getAllMoves board pawnMoves coord (length pawnMoves) valid
    where   piece = typeOf (pieceOf (getFromBoard board coord))
            player = playerOf (pieceOf (getFromBoard board coord))
            knightMoves = getKnightMoves
            pawnMoves = getPawnMoves player valid

-- | Returns all the moves that a piece is able to make
getAllMoves :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> Bool -> [(Int, Int)]
getAllMoves board move coord 0 valid = []
getAllMoves board (move:restOfMoves) coord index valid 
    | (valid && checkBounds coord && checkBounds to && getFromBoard board to == E) = to : (getAllMoves board restOfMoves coord (index - 1) valid)
    | ((not valid) && checkBounds coord && checkBounds to) = to : (getAllMoves board restOfMoves coord (index - 1) valid)
    | otherwise = getAllMoves board restOfMoves coord (index - 1) valid
    where to = (fst coord + fst move, snd coord + snd move)
    
-- |a list that contains the allowed moveset for knight(the L shape)
getKnightMoves :: [(Int, Int)]
getKnightMoves = [(1, 2), 
                (1, -2), 
                (-1, 2), 
                (-1, -2), 
                (2, 1), 
                (2, -1), 
                (-2, 1), 
                (-2, -1)]

-- |returns the moves that a pawn is allowed to make, including diagonal if attacking
getPawnMoves :: Player -> Bool -> [(Int, Int)]
getPawnMoves Black True  = [(1, -1), (-1, -1)]
getPawnMoves White True  = [(1, 1), (-1, 1)]
getPawnMoves Black False = [(0, -1)]
getPawnMoves White False = [(0, 1)]

-- |Data type of pieces that does not account for piece ownership
data PieceType = Knight | Pawn deriving (Eq, Show, Read)

-- |Simple function that shows the type of a piece, excluding colour
typeOf :: Piece -> PieceType
typeOf BlackKnight = Knight
typeOf BlackPawn   = Pawn
typeOf WhiteKnight = Knight
typeOf WhitePawn   = Pawn

-- |Simple function that returns a list of 2-tuple coordinates of every piece of the player's
getMyPieces :: Board -> Player -> [(Int, Int)]
getMyPieces board playerType = (coordsOfPieceType board playerType Knight) ++ (coordsOfPieceType board playerType Pawn)

-- |Simple function that returns the opponent's colour(to determine piece ally/enemy)
getOpponent :: Player -> Player
getOpponent Black = White
getOpponent White = Black

-- |Simple check to see if a coordinate is within the boundaries of the board
checkBounds      :: (Int, Int) -> Bool
checkBounds move 
    | a>4 = False
    | b>4 = False
    | a<0 = False
    | b<0 = False
    | otherwise = True
    where   a = fst move
            b = snd move
