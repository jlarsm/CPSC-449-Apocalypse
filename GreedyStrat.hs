{-|
Module      :   GreedyStrat
Description :   A Greedy strategy that will take the first piece it sees, first with the knights then
                with pawns, then any non-attacking moves after that
Stability   :   experimental
Portability :   ghc 7.10.2 - 7.10.3
-}
module GreedyStrat where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import StrategyFunctions
import ApocTools

-- |The greedy AI of type Chooser, computes a move to play on the board
greedy :: Chooser
greedy gameState Normal player =
    let board = theBoard gameState in
    let regularMoves = (orderRegularMoves board (getMyPieces board player)) in
    let pawnAttackMoves = (listAttackMoves board (coordsOfPieceType board player Pawn)) in
    let knightAttackMoves = (listAttackMoves board (coordsOfPieceType board player Knight)) in
    let move = randomlyPickFromList (regularMoves ++ pawnAttackMoves ++ knightAttackMoves) 0.65 in
    if move == Nothing
    then return Nothing
    else return $ Just [(fst (fromJust move)), (snd (fromJust move))]
greedy gameState PawnPlacement player =
  let emptyPieces = coordinatesOf (theBoard gameState) E 0 0 0 1 4 3 in
  let move = pickFromList emptyPieces in
  if move == Nothing
  then return Nothing
  else return $ Just [(fromJust move)]

-- |Returns any regular move that will not lead to taking any opponent pieces
orderRegularMoves :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
orderRegularMoves board [] = []
orderRegularMoves board (coord:coords)
    | (best == Nothing) = (orderRegularMoves board coords)
    | otherwise = (coord, fromJust best) : (orderRegularMoves board coords)
    where   movesList = allRegulars board coord
            best = randomlyPickFromList movesList 0.65
                                 
-- |Sorts a list of attack possibilities, placing attacking a knight on high priority
sortAttacks :: Board -> [(Int, Int)] -> [(Int, Int)]
sortAttacks board [] = []
sortAttacks board (x:xs) 
    | (piece == Knight) = [x] ++ (sortAttacks board xs)
    | (piece == Pawn)   = (sortAttacks board xs) ++ [x]
    where piece = typeOf (pieceOf (getFromBoard board x))
                        
-- |Returns a sorted list of moves that will result in the specified piece attacking any enemy piece
listAttackMoves :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
listAttackMoves board [] = []
listAttackMoves board (coord:coords)
    | (bestAttackPiece == Nothing)         = (listAttackMoves board coords)
    | (fromJust bestAttackPiece == Knight) = [(coord, fromJust bestAttack)] ++ listAttackMoves board coords
    | (fromJust bestAttackPiece == Pawn)   = listAttackMoves board coords ++ [(coord, fromJust bestAttack)]
    where killsList = allAttacks board coord
          bestAttack = randomlyPickFromList (sortAttacks board killsList) 0.65
          bestAttackPiece   | (bestAttack == Nothing) = Nothing
                            | otherwise = Just $ typeOf (pieceOf (getFromBoard board (fromJust bestAttack)))
