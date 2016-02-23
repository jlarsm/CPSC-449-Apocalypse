{-|
Module      :   DefensiveStrat
Description :   A greedy strategy that will prioritize keeping allied pieces alive
                over taking enemy pieces
Stability   :   experimental
Portability :   ghc 7.10.2 - 7.10.3
-}
module DefensiveStrat where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import System.Environment
import System.Random
import StrategyFunctions
import ApocTools

defensive :: Chooser
defensive gameState Normal player =
    let board = theBoard gameState in
    let saviourMoves = (listMovesToSafety board player (getPiecesAtRisk board player)) in
    if (length saviourMoves > 0) && coinFlip
    then let move = randomlyPickFromList saviourMoves 0.8 in
        if move == Nothing
        then return Nothing
        else return $ Just [(fst (fromJust move)), (snd (fromJust move))]
    else
        let regularMoves = (orderRegularMoves board player (getMyPieces board player))
            pawnAttackMoves = (listAttackMoves board player (coordsOfPieceType board player Pawn))
            knightAttackMoves = (listAttackMoves board player (coordsOfPieceType board player Knight))
            move = randomlyPickFromList (regularMoves ++ pawnAttackMoves ++ knightAttackMoves) 0.8 in
        if move == Nothing 
        then return Nothing
        else return $ Just [(fst (fromJust move)), (snd (fromJust move))]
defensive gameState PawnPlacement player =
  let emptyPieces = getSafeCoords (theBoard gameState) player (coordinatesOf (theBoard gameState) E 0 0 0 1 4 3) in
  let move = pickFromList emptyPieces in
  if move == Nothing
  then return Nothing
  else return $ Just [(fromJust move)]
  
-- |Returns a list of moves that would not lead into a risky spot
sortRegulars :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
sortRegulars board player [] = []
sortRegulars board player (coord:coords) 
    | isSafe board player coord = coord : (sortRegulars board player coords)
    | otherwise = (sortRegulars board player coords) ++ [coord]
                         
-- |Returns a sorted list of regular moves that will not lead to taking any opponent pieces
-- nor will it lead to placing a piece in a risky spot
orderRegularMoves :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
orderRegularMoves board player [] = []
orderRegularMoves board player (coord:coords) 
    | (best == Nothing) = (orderRegularMoves board player coords)
    | otherwise = (coord, fromJust best) : (orderRegularMoves board player coords)
    where   movesList = allRegulars board coord
            best = randomlyPickFromList (sortRegulars board player movesList) 0.8

-- |Sorts a list of attack possibilities, placing attacking a knight on higher priority
sortAttacks :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
sortAttacks board player [] = []
sortAttacks board player (x:xs) | isSafe board player x = x : (sortAttacks board player xs)
                        | (piece == Knight) = (sortAttacks board player xs) ++ [x]
                        | (piece == Pawn)   = (sortAttacks board player xs) ++ [x]
                        where piece = typeOf (pieceOf (getFromBoard board x))
                        
-- |Returns a sorted list of moves that will result in the specified piece attacking any enemy piece
listAttackMoves :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
listAttackMoves board player [] = []
listAttackMoves board player (coord:coords)
    | (bestAttackPiece == Nothing) = (listAttackMoves board player coords)
    | (isSafe board player (fromJust bestAttack)) = [(coord, fromJust bestAttack)] ++ listAttackMoves board player coords
    | (fromJust bestAttackPiece == Knight) = listAttackMoves board player coords ++ [(coord, fromJust bestAttack)]
    | (fromJust bestAttackPiece == Pawn)   = listAttackMoves board player coords ++ [(coord, fromJust bestAttack)]
    where killsList = allAttacks board coord
          bestAttack = randomlyPickFromList (sortAttacks board player killsList) 0.5
          bestAttackPiece   | (bestAttack == Nothing) = Nothing
                            | otherwise = Just $ typeOf (pieceOf (getFromBoard board (fromJust bestAttack)))
   
-- |Returns a list containing all your pieces that are at risk
getPiecesAtRisk :: Board -> Player -> [(Int, Int)]
getPiecesAtRisk board player = allPiecesAtRisk board player ((coordsOfPieceType board player Pawn)
                                ++ (coordsOfPieceType board player Knight))

-- |Returns a list of the player's pieces that are in danger
allPiecesAtRisk :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
allPiecesAtRisk board player [] = []
allPiecesAtRisk board player (x:xs)
    | not (isSafe board player x) = x : allPiecesAtRisk board player xs
    | otherwise = allPiecesAtRisk board player xs

-- |Returns all the coordinates that are considered safe to move to
getSafeCoords :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
getSafeCoords board player [] = []
getSafeCoords board player (x:xs)
    | (isSafe board player x) = x : getSafeCoords board player xs
    | otherwise = getSafeCoords board player xs
    
-- |Returns a list of coords that are considered safe for the player, as in no immediate threat of 
-- enemy piece attacking
listSafeSpots :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
listSafeSpots board player [] = []
listSafeSpots board player (move:restOfMoves)
    | not (isSafe board player move) = move : listSafeSpots board player restOfMoves
    | otherwise = listSafeSpots board player restOfMoves
                           
-- |Returns a list of moves that will remove a piece from a risky spot
listMovesToSafety :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
listMovesToSafety board player [] = []
listMovesToSafety board player (coord:coords)
    | (bestMove == Nothing) = listMovesToSafety board player coords
    | otherwise = (coord, fromJust bestMove) : listMovesToSafety board player coords
    where   movesList = getMoves board coord False
            bestMove = randomlyPickFromList (listSafeSpots board player movesList) 0.8
