{- | This module is used for CPSC 449 for the Apocalypse assignment.
Feel free to modify this file as you see fit.
Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.
-}

module Main(main) where

import System.Random
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.List
import Data.Char
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
        args <- getArgs
        if args == [] then do
          putStrLn "Possible strategies:\n human\n greedy"
          putStrLn "Choose strategy for BLACK:"
          play <- getLine
          bStrat <- (check play)
          putStrLn "Choose strategy for WHITE:"
          play1 <- getLine
          wStrat <- (check play1)
          putStrLn "The initial board:"
          print initBoard
          gameLoop initBoard bStrat wStrat

        else do 
            if not (("easy" `elem` args && "human" `elem` args) ||  ("easy" `elem` args && "greedy" `elem` args) || ("greedy" `elem` args && "human" `elem` args) || (args == ["easy", "easy"]) || (args == ["greedy", "greedy"]) || (args == ["human", "human"])) then do
              putStrLn "Invalid strategy"
            else do
              putStrLn "The initial board:"
              print initBoard
---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

check :: String -> IO Chooser
check n 
  | n == "human" = return human 
  | n == "greedy" = return human
  |otherwise = do putStrLn "Invalid input"; ply <- getLine; check ply
  
gameLoop :: GameState -> Chooser -> Chooser -> IO ()
gameLoop board bStrat wStrat = do
    bMove <- bStrat board Normal Black
    wMove <- wStrat board Normal White
    let nextState = GameState (if bMove == Nothing 
                                then Passed 
                                else if(validateMove board (fromJust bMove) Black)
                                then Played (head (fromJust bMove), head (tail (fromJust bMove)))
                                else Goofed (head (fromJust bMove), head (tail (fromJust bMove))))
                                (blackPen initBoard)
                                (if wMove == Nothing 
                                then Passed 
                                else if(validateMove board (fromJust wMove) White)
                                then Played (head (fromJust wMove), head (tail (fromJust wMove)))
                                else Goofed (head (fromJust wMove), head (tail (fromJust wMove))))
                                (whitePen initBoard)
                                (if bMove == Nothing
                                  then (if wMove == Nothing 
                                        then do theBoard board
                                        else (replace2 (replace2 (theBoard board)((fromJust wMove) !! 1)(getFromBoard (theBoard board) ((fromJust wMove) !! 0)))((fromJust wMove) !! 0)E))
                                  else (if wMove == Nothing then (replace2 (replace2 (theBoard board)((fromJust bMove) !! 1)(getFromBoard (theBoard board) ((fromJust bMove) !! 0)))((fromJust bMove) !! 0)E)
                                        else (replace2 (replace2 (replace2 (replace2 (theBoard board) ((fromJust wMove) !! 0) E) ((fromJust bMove) !! 0) E) ((fromJust wMove) !! 1) (getFromBoard (theBoard board) ((fromJust wMove) !! 0))) ((fromJust bMove) !! 1) (getFromBoard (theBoard board) ((fromJust bMove) !! 0)))))            
    if (isGameOver nextState bMove wMove) == True then gameOver nextState
      else do
    putStrLn (show nextState)
    gameLoop nextState bStrat wStrat
    
isGameOver :: GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> Bool
isGameOver board bMove wMove
                      | bMove == Nothing && wMove == Nothing = True
                      | elem '/' (board2Str (theBoard board)) == False = True
                      | elem '+' (board2Str (theBoard board)) == False = True
                      | (blackPen board) >= 2 || (whitePen board) >= 2 = True
                      | otherwise = False
                      
gameOver :: GameState -> IO()
gameOver board = putStrLn ("Winner is:" ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])

--Logic that handles if a clash has happened on the board, takes in the game state
--WhitePlayed first, and then BlackPlayed second, returns what the cell should be
handleClash:: GameState -> Played -> Played -> Cell
handleClash board (Played(startW,endW)) (Played(startB,endB)) = do
    if(getFromBoard (theBoard board) startW==WP && getFromBoard (theBoard board) startB==BP)
    then E
    else if(getFromBoard (theBoard board) startW==WP && getFromBoard (theBoard board) startB==BK)
    then BK
    else if(getFromBoard (theBoard board) startW==WK && getFromBoard (theBoard board) startB==BP)
    then WK
    else if(getFromBoard (theBoard board) startW==WK && getFromBoard (theBoard board) startB==BK)
    then E
    else E
    
--takes in the board, list of tuples indicating the move, Player("Black" or "White")
validateMove :: GameState -> [(Int,Int)] -> Player -> Bool
validateMove board [(x1,y1),(x2,y2)] Black --for black pieces
    |getFromBoard (theBoard board) (x1,y1) == BP && x1==x2 && y1-1==y2 
        && getFromBoard (theBoard board) (x2,y2)== E = True
    |getFromBoard (theBoard board) (x1,y1) == BP && x1+1==x2 && y1-1==y2 
        && (getFromBoard (theBoard board) (x2,y2)== WK || getFromBoard (theBoard board) (x2,y2)== WP) = True
    |getFromBoard (theBoard board) (x1,y1) == BP && x1-1==x2 && y1-1==y2
        && (getFromBoard (theBoard board) (x2,y2)== WK || getFromBoard (theBoard board) (x2,y2)== WP) = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1+1 == x2 && y1+2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1-1 == x2 && y1+2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1+1 == x2 && y1-2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1-1 == x2 && y1-2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1+2 == x2 && y1+1 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1-2 == x2 && y1+1 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1+2 == x2 && y1-1 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == BK && x1-2 == x2 && y1-1 == y2 = True
    | otherwise = False
validateMove board [(x1,y1),(x2,y2)] White --for white pieces
    |getFromBoard (theBoard board) (x1,y1) == WP && x1==x2 && y1+1==y2 
        && getFromBoard (theBoard board) (x2,y2)== E = True
    |getFromBoard (theBoard board) (x1,y1) == WP && x1+1==x2 && y1+1==y2 
        && (getFromBoard (theBoard board) (x2,y2)== BK || getFromBoard (theBoard board) (x2,y2)== BP) = True
    |getFromBoard (theBoard board) (x1,y1) == WP && x1-1==x2 && y1+1==y2
        && (getFromBoard (theBoard board) (x2,y2)== BK || getFromBoard (theBoard board) (x2,y2)== BP) = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1+1 == x2 && y1+2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1-1 == x2 && y1+2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1+1 == x2 && y1-2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1-1 == x2 && y1-2 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1+2 == x2 && y1+1 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1-2 == x2 && y1+1 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1+2 == x2 && y1-1 == y2 = True
    |getFromBoard (theBoard board) (x1,y1) == WK && x1-2 == x2 && y1-1 == y2 = True
    | otherwise = False
    
