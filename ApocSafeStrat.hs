module ApocSafeStrat where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import System.Environment
import Data.List
import Data.Char
import ApocTools
import System.Random
import ApocStrategyHuman
import ApocGreedyStrat


-- This is our safe strat. At the core it is similar to the greedy strat, the main difference lies in the ai
-- moving away from the opponents pieces as opposed to moving towards kills



safe :: Chooser

safe gameState Normal player =
        let board = theBoard gameState in
		let safeMoves = (safeMovesOrdered board player (dangerPieces board player)) in
		if (length safeMoves > 0) then
		    let move = chooseFromList safeMoves in
		    if move /= Nothing then
		        return $ Just [(fst (fromJust move)), (snd (fromJust move))]
		    else return Nothing
		else 
		    let killMovesOne = (killMovesOrdered board (allPiecesOfType board player Knight)) in
	        let killMovesTwo = (killMovesOrdered board (allPiecesOfType board player Pawn)) in
	        let notKillMoves = (notKillMovesOrdered board (allPieces board player)) in
	        let move = chooseFromList (killMovesOne ++ killMovesTwo ++ notKillMoves) in
	        if move /= Nothing then
		        return $ Just [(fst (fromJust move)), (snd (fromJust move))]
	    else return Nothing

safe gameState PawnPlacement player =
        let spaces = allPiecesOfType (theBoard gameState) in
	    let move = chooseFromList spaces in
	    if move /= Nothing then
		    return $ Just [(fst (fromJust move)), (snd (fromJust move))]
	    else return Nothing		
		
		
		
		
otherPlayer :: player -> player
otherPlayer Black = White
otherPlayer White = Black	


safePlace :: Board -> Player -> (Int, Int) -> Bool
safePlace b player p = safePlacePostion (move b (otherPlayer player)) p

safePlacePostion :: [(Int, Int)] -> (Int, Int) -> Bool
safePlacePostion [] pos = True
safePlacePostion (x:xs) pos | (x == pos) = False
                            | otherwise = safePlacePostion xs pos
							

getDanger :: Board -> Player -> [(Int, Int)] 
getDanger b player = dangerPieces b player ((specificPiece b player Pawn) ++ (specificPiece b player Knight))

dangerPieces :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
dangerPieces b player [] = []
dangerPieces b player (x:xs) | not (safePlace b player x) = x : dangerPieces b player xs
                             | otherwise = dangerPieces b player xs

safeMovesOrdered :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
safeMovesOrdered b player [] = []
safeMovesOrdered b player (x:xs) = safeMovesOrdered b player xs