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
                                else Played (head (fromJust bMove), head (tail (fromJust bMove))))
                                (blackPen initBoard)
                                (if wMove == Nothing 
                                then Passed
                                else Played (head (fromJust wMove), head (tail (fromJust wMove))))
                                (whitePen initBoard)
                                (if bMove == Nothing
                                  then (if wMove == Nothing 
                                        then do theBoard board
                                        else (replace2 (replace2 (theBoard board)((fromJust wMove) !! 1)(getFromBoard (theBoard board) ((fromJust wMove) !! 0)))((fromJust wMove) !! 0)E))
                                  else (if wMove == Nothing then (replace2 (replace2 (theBoard board)((fromJust bMove) !! 1)(getFromBoard (theBoard board) ((fromJust bMove) !! 0)))((fromJust bMove) !! 0)E)
                                        else (replace2 (replace2 (replace2 (replace2 (theBoard board) ((fromJust wMove) !! 0) E) ((fromJust bMove) !! 0) E) ((fromJust wMove) !! 1) (getFromBoard (theBoard board) ((fromJust wMove) !! 0))) ((fromJust bMove) !! 1) (getFromBoard (theBoard board) ((fromJust bMove) !! 0)))))            
    putStrLn (show nextState)
    if (isGameOver nextState bMove wMove) == True then gameOver nextState
      else
        if checkEnd nextState
          then placer nextState bStrat wStrat
          else gameLoop nextState bStrat wStrat
isGameOver :: GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> Bool
isGameOver board bMove wMove
                      | bMove == Nothing && wMove == Nothing = True
                      | elem '/' (board2Str (theBoard board)) == False = True
                      | elem '+' (board2Str (theBoard board)) == False = True
                      | (blackPen board) >= 2 || (whitePen board) >= 2 = True
                      | otherwise = False
gameOver :: GameState -> IO() --needs chooser2string method for putstrln
gameOver board 
              | [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))] > [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] = putStrLn ("Winner is: WHITE" ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])
              | [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))] < [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] = putStrLn ("Winner is: BLACK" ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])
              | [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))] == [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] = putStrLn ("DRAW! " ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])

checkEnd :: GameState -> Bool
checkEnd board = if elem BP (head(theBoard board)) || elem WP (last(theBoard board)) 
        then True
        else False
promotion :: Board -> Player -> Maybe[(Int,Int)] -> Board
promotion board Black move = if move == Nothing && (length(findIndices (== '#') (board2Str board))) >= 2 then replace2 board ((findPawn (head board) BP),0) BK
                              else replace2 (replace2 board ((fromJust move) !! 0) BP) ((findPawn (head board) BP),0) E

promotion board White move = if move == Nothing && (length(findIndices (== 'X') (board2Str board))) >= 2 then replace2 board ((findPawn (last board) WP),4) WK
                              else replace2 (replace2 board ((fromJust move) !! 0) WP) ((findPawn (last board) WP),4) E

placer :: GameState -> Chooser -> Chooser -> IO()
placer board bStrat wStrat = do
    let b = elem BP (head(theBoard board))
    let w = elem WP (last(theBoard board))
    bMove <- if b && length(findIndices (== '#') (board2Str (theBoard board))) >= 2
      then bStrat board PawnPlacement Black 
      else return Nothing
    wMove <- if w && length(findIndices (== 'X') (board2Str (theBoard board))) >= 2
      then wStrat board PawnPlacement White 
      else return Nothing
    let nextState = GameState (if b == False
                                 then None
                                 else if bMove == Nothing
                                    then if length(findIndices (== '#') (board2Str (theBoard board))) < 2
                                        then UpgradedPawn2Knight ((findPawn (head(theBoard board)) BP),0)
                                        else NullPlacedPawn 
                                    else if isValid bMove board
                                        then PlacedPawn (((findPawn (head(theBoard board)) BP),0), ((fromJust bMove) !! 0))
                                        else BadPlacedPawn (((findPawn (head(theBoard board)) BP),0), ((fromJust bMove) !! 0)))
                              (blackPen board + if (b && (bMove == Nothing && length(findIndices (== '#') (board2Str (theBoard board))) >= 2) || isValid bMove board == False) then 1 else 0)
                              (if w == False
                                 then None
                                 else if wMove == Nothing
                                    then if length(findIndices (== 'X') (board2Str (theBoard board))) < 2
                                        then UpgradedPawn2Knight ((findPawn (last(theBoard board)) WP),4)
                                        else NullPlacedPawn 
                                    else if isValid wMove board
                                        then PlacedPawn (((findPawn (last(theBoard board)) WP),4), ((fromJust wMove) !! 0))
                                        else BadPlacedPawn (((findPawn (last(theBoard board)) WP),4), ((fromJust wMove) !! 0)))
                              (whitePen board + if (w && (wMove == Nothing && length(findIndices (== 'X') (board2Str (theBoard board))) >= 2) || isValid wMove board == False) then 1 else 0)
                              (if b && isValid bMove board
                                  then if w && isValid wMove board
                                    then if (length(findIndices (== '#') (board2Str (theBoard board))) >= 2 && 
                                            length(findIndices (== 'X') (board2Str (theBoard board))) >= 2 && 
                                            isMoveEqual bMove wMove)
                                        then (replace2 (replace2 (theBoard board) ((findPawn (head(theBoard board)) BP),0) E) ((findPawn (last(theBoard board)) WP),4) E)
                                        else do promotion (promotion (theBoard board) White wMove) Black bMove
                                    else if length(findIndices (== '#') (board2Str (theBoard board))) >= 2 
                                      then theBoard board
                                      else promotion (theBoard board) Black bMove
                                  else if w && isValid wMove board
                                    then  promotion (theBoard board) White wMove 
                                    else (theBoard board))
    putStrLn (show nextState)
    gameLoop nextState bStrat wStrat 

findPawn :: [Cell] -> Cell -> Int
findPawn (x:xs) c = if x == c 
                      then 0
                      else 1 + findPawn xs c 
isValid :: Maybe[(Int,Int)] -> GameState -> Bool
isValid move board 
              | move == Nothing = True
              | (getFromBoard (theBoard board) ((fromJust move) !! 0)) == E = True
              | otherwise = False
isMoveEqual :: Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> Bool
isMoveEqual move1 move2 
              | move1 == Nothing && move2 == Nothing = True
              | (head(fromJust move1)) == (head(fromJust move2)) = True
              | otherwise = False
-- | Checks the (x,y)th element in a 2d list and returns the element without changes.
checkLocation2d :: [[a]] -> (Int,Int) -> a
checkLocation2d (x:xs) (0,b) = checkLocation x b
checkLocation2d (x:xs) (a,b) = checkLocation2d xs (a-1,b)

checkLocation :: [a] -> Int -> a
checkLocation (x:xs) 0 = x 
checkLocation (x:xs) n = checkLocation xs (n-1)

    
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
    
