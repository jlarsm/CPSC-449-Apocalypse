module ApocGreedyStrat where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import System.Environment
import Data.List
import Data.Char
import ApocTools
import System.Random
import ApocStrategyHuman



greedy :: Chooser

greedy gameState Normal player =
	let board = theBoard gameState in	
	
	let killMovesOne = (killMovesOrdered board (allPiecesOfType board player Knight)) in
	let killMovesTwo = (killMovesOrdered board (allPiecesOfType board player Pawn)) in
	let notKillMoves = (notKillMovesOrdered board (allPieces board player)) in
	let move = chooseFromList (killMovesOne ++ killMovesTwo ++ notKillMoves) in
	if move /= Nothing then
		return $ Just [(fst (fromJust move)), (snd (fromJust move))]
	else return Nothing
	
	

	
greedy gameState PawnPlacement player =

    let spaces = pieces (theBoard gameState) E 1 1 1 1 1 1 in
	let move = chooseFromList spaces in
	if move /= Nothing then
		return $ Just [(fromJust move)]
	else return Nothing

-- Pieces --

data PiecesKP = Knight | Pawn deriving (Eq, Show, Read)

specificPiece :: Piece -> PiecesKP
specificPiece WhiteKnight = Knight
specificPiece BlackKnight = Knight
specificPiece WhitePawn = Pawn
specificPiece BlackPawn = Pawn


-- List of all coords that belong to each individual player --
allPieces :: Board -> Player -> [(Int, Int)]
allPieces b p = (allPiecesOfType b p Knight) ++ (allPiecesOfType b p Pawn)

allPiecesOfType :: Board -> Player -> PiecesKP -> [(Int, Int)]
allPiecesOfType b p t | (p == White && t == Knight) = pieces b WK 0 0 0 0 4 4
                      | (p == White && t == Pawn)   = pieces b WP 0 0 0 0 4 4
                      | (p == Black && t == Knight) = pieces b BK 0 0 0 0 4 4
                      | (p == Black && t == Pawn)   = pieces b BP 0 0 0 0 4 4
					  
pieces :: Board -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
pieces b p x y minx miny maxx maxy | (x == maxx && y == maxy) = if (getFromBoard b (maxx, maxy)) == p then (x, y) : [] else []
                                   | (x == maxx) = if (getFromBoard b (maxx, y)) == p then (maxx, y) : pieces b p minx (y + 1) minx miny maxx maxy else pieces b p minx (y + 1) minx miny maxx maxy
                                   | otherwise = if (getFromBoard b (x, y)) == p then (x, y) : pieces b p (x + 1) y minx miny maxx maxy else pieces b p (x + 1) y minx miny maxx maxy


--Moves--

-- Knight Options --

knightOptions :: [(Int, Int)]
knightOptions = [(1,2), (1,-2), (-1, 2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]

-- Returns True or False based on whether the pawn can kill or not --

pawnOptions :: Player -> Bool -> [(Int, Int)]
pawnOptions Black True = [(1,1), (-1,1)]
pawnOptions Black False = [(0,1)]
pawnOptions White True = [(-1,-1), (1,-1)]
pawnOptions White False =  [(0,-1)]


-- randomly choose element from list --

chooseFromList :: [a] -> Maybe a
chooseFromList [] = Nothing
chooseFromList xs = Just (xs !! index)
                  where index = floor ((unsafePerformIO (randomIO :: IO Float)) * (fromIntegral (length xs)))

			
			
-- Checks if a move is a possible kill or not --
			
moveKill :: Board -> (Int,Int) -> (Int,Int) -> Bool
moveKill s from to = (boundCheck to && boundCheck from && getFromBoard s to /= E && (playerOf (pieceOf (getFromBoard s to))) /= (playerOf (pieceOf (getFromBoard s from))))

-- List of all kill moves a piece can make --

allKills :: Board -> (Int, Int) -> [(Int, Int)]
allKills b pos  | (piece == Knight) = kill b knightMoves pos (length knightMoves)
                | (piece == Pawn)   = kill b pawnMoves pos (length pawnMoves)
                | otherwise = []
                where piece = specificPiece(pieceOf (getFromBoard b pos))
                      player = playerOf (pieceOf (getFromBoard b pos))
                      knightMoves = knightOptions
                      pawnMoves = pawnOptions player True


kill :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
kill a (b:bs) pos index | moveKill a pos to = to : (kill a bs pos (index - 1))
						| otherwise = kill a bs pos (index - 1)
						where to = (fst pos + fst b, snd pos + snd b)
						
killMovesOrdered :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
killMovesOrdered b [] = []
killMovesOrdered b (x:xs) | (bestKillPiece == Nothing)         = (killMovesOrdered b xs)
                        | (fromJust bestKillPiece == Knight) = [(x, fromJust bestKill)] ++ killMovesOrdered b xs
                        | (fromJust bestKillPiece == Pawn)   = killMovesOrdered b xs ++ [(x, fromJust bestKill)]
                        where killsList = allKills b x
                              bestKill = chooseFromList (killOrders b killsList)
                              bestKillPiece | (bestKill == Nothing) = Nothing
                                            | otherwise = Just $ specificPiece (pieceOf (getFromBoard b (fromJust bestKill)))

-- |Orders a list of "kill" moves based on whether they kill a Knight or a Pawn
killOrders :: Board -> [(Int, Int)] -> [(Int, Int)]
killOrders board [] = []
killOrders board (k:ks) | (piece == Knight) = [k] ++ (killOrders board ks)
                        | (piece == Pawn)   = (killOrders board ks) ++ [k]
                        where piece = specificPiece (pieceOf (getFromBoard board k))
						
						
						
allNotKills :: Board -> (Int, Int) -> [(Int, Int)]
allNotKills b pos | (piece == Knight) = notKill b knightMoves pos (length knightMoves)
                  | (piece == Pawn)   = notKill b pawnMoves pos (length pawnMoves)
                  where piece = specificPiece (pieceOf (getFromBoard b pos))
                        player = playerOf (pieceOf (getFromBoard b pos))
                        knightMoves = knightOptions
                        pawnMoves = pawnOptions player False		
						
				
notKillMovesOrdered :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
notKillMovesOrdered b [] = []
notKillMovesOrdered b (x:xs) | (best == Nothing) = (notKillMovesOrdered b xs)
                           | otherwise = (x, fromJust best) : (notKillMovesOrdered b xs)
                           where movesList = allNotKills b x
                                 best = chooseFromList (notKills2 b movesList)
								 
notKills2 :: Board -> [(Int, Int)] -> [(Int, Int)]
notKills2 b xs = xs
						
						
-- Checks if it is not a kill move --

notKillMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
notKillMove s from to	| (boundCheck to && boundCheck from && getFromBoard s to == E) = True
						| otherwise = False
				  

-- List of all not kill moves --

notKill :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
notKill a b c 0 = []
notKill a (b:bs) pos index 	| notKillMove a pos to = to : (notKill a bs pos (index - 1))
							| otherwise = notKill a bs pos (index - 1)
							where to = (fst pos + fst b, snd pos + snd b)





boundCheck      :: (Int, Int) -> Bool
boundCheck move | a>4 = False
                | b>4 = False
                | a<0 = False
                | b<0 = False
                | otherwise = True
                where a = fst move
                      b = snd move











