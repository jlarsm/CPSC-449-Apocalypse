module ApocGreedyStrat where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import System.Environment
import Data.List
import Data.Char
import ApocTools
import Random



greedy :: Chooser

greedy gameState Normal player =
	let board = board gameState in	
	
	let kill = (kill board (pieceOf board player)) in
	let notKill = (notKill board (pieceOf board player)) in
	
	let move = chooseFromList (kill + noKill) in
	
	if move /= Nothing then
		return $ Just [(first (fromJust move)), (second (fromJust move))]
	else return Nothing
	
	

	
greedy gameState PawnPlacement player = return $ Just [(2,2)]

--Moves--

knightOptions :: [(Int, Int)]
knightOptions = [(1,2), (1,-2), (-1, 2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]


pawnOptions :: Player -> [(Int, Int)]
pawnOptions Black = [(1,1), (-1,1), (0,1)]
pawnOptions White = [(-1,-1), (1,-1), (0,-1)]


-- randomly choose element from list --

chooseFromList :: [a] -> IO a
chooseFromList [] = Nothing
chooseFromList (x:[]) = Just x
chooseFromList (x:xs)	| rand <= 1 = Just xs
						| otherwise = chooseFromList xs
						where rand = randomIO :: IO Float





			
			
-- Checks if a move is a possible kill or not --
			
moveKill :: Board -> (Int,Int) -> (Int,Int) -> Bool
moveKill s from to = (boundCheck to && boundCheck from && getFromBoard s to /= E && (playerOf (pieceOf (getFromBoard s to))) /= (playerOf (pieceOf (getFromBoard s from))))

-- List of all kill moves a piece can make --

kill :: Board [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
kill a b c = []
kill a (b:bs) pos index | moveKill a pos to = to : (kill a bs pos (index - 1))
						| otherwise = kill a bs pos (index - 1)
						where to = (first pos + first b, second pos + second b)
						
						
						
-- Checks if it is not a kill move --

moveNotKill :: Board -> (Int, Int) -> (Int, Int) -> Bool
moveNotKill s from to	| (boundCheck to && boundCheck from && getFromBoard s to == E) = True
						| otherwise = False
				  

-- List of all not kill moves --

notKill :: Board -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
notKill a b c = []
notKill a (b:bs) pos index 	| moveNotKill a pos to = to : (notKill a bs pos (index - 1))
							| otherwise = notKill a bs pos (index - 1)
							where to = (first pos + first b, second pos + second b)





boundCheck      :: (Int, Int) -> Bool
boundCheck move | x<0 = False
				| x>4 = False
				| y<0 = False
				| y>4 = False
				| otherwise = True
				where x = first move
					  y = second move











