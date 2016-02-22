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
		if (lenght safeMoves > 0) then
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
		
		
		
		
		
		
		
		
		
		
		
		
		
		
-- Below is code copy pasted from greedy Strat. It shows a list of moves that can result in kills/not kills		
		
killMovesOrdered :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
killMovesOrdered b [] = []
killMovesOrdered b (p:ps) | (bestKillPiece == Nothing)         = (killMovesOrdered b ps)
                        | (fromJust bestKillPiece == Knight) = [(p, fromJust bestKill)] ++ killMovesOrdered b ps
                        | (fromJust bestKillPiece == Pawn)   = killMovesOrdered b ps ++ [(p, fromJust bestKill)]
                        where killsList = allKills b p
                              bestKill = chooseFromList (killOrders b killsList)
                              bestKillPiece | (bestKill == Nothing) = Nothing
                                            | otherwise = Just $ specificPiece (pieceOf (getFromBoard b (fromJust bestKill)))

-- |Orders a list of "kill" moves based on whether they kill a Knight or a Pawn
killOrders :: Board -> [(Int, Int)] -> [(Int, Int)]
killOrders board [] = []
killOrders board (k:ks) | (piece == Knight) = [k] ++ (killOrders board ks)
                        | (piece == Pawn)   = (killOrders board ks) ++ [k]
                        where piece = specificPiece (pieceOf (getFromBoard board k))	
				
notKillMovesOrdered :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
notKillMovesOrdered b [] = []
notKillMovesOrdered b (p:ps) | (best == Nothing) = (notKillMovesOrdered b ps)
                           | otherwise = (p, fromJust best) : (notKillMovesOrdered b ps)
                           where movesList = allNotKills b p
                                 best = chooseFromList (orderNotKills b movesList)