{-|
Module      :   Apoc
Description :   Main module for the CPSC449 Winter 2016 semester's Haskell
                assignment(a chess variant).
Stability   :   experimental
Portability :   ghc 7.10.2 - 7.10.3
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
import HumanStrat
import GreedyStrat
import DefensiveStrat


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
          putStrLn "Possible strategies:\n  human\n  greedy\n  defensive"
          putStrLn "Choose strategy for BLACK:"
          play <- getLine
          bStrat <- (check play)
          putStrLn "Choose strategy for WHITE:"
          play1 <- getLine
          wStrat <- (check play1)
          putStrLn "The initial board:"
          print initBoard
          gameLoop initBoard bStrat wStrat play play1
        else do 
            if not (("easy" `elem` args && "human" `elem` args) ||  ("easy" `elem` args && "greedy" `elem` args) || ("greedy" `elem` args && "human" `elem` args) || (args == ["easy", "easy"]) || (args == ["greedy", "greedy"]) || (args == ["human", "human"])) then do
              putStrLn "Invalid Strategy\nPossible Strategies:\n  human\n  greedy\n  defensive"
            else do
              putStrLn "The initial board:"
              print initBoard
              bStrat <- check (args !! 0)
              wStrat <- check (args !! 1)
              gameLoop initBoard bStrat wStrat (args !! 0) (args !! 1)
{- |2D list utility functions
-}

-- |Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- |Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

-- |Checks and confirms the inputted decision for which strategy to use
check :: String -> IO Chooser
check n 
    | n == "human" = return human 
    | n == "greedy" = return greedy
    | n == "defensive" = return defensive
    |otherwise = do putStrLn "Invalid input\nPossible Strategies:\n  human\n  greedy\n  defensive"; ply <- getLine; check ply

-- |The main function for the actual game itself, once everything's been set up takes in
--  the inputted move each turn and performs the required logic on it to move to the next turn
gameLoop :: GameState -> Chooser -> Chooser -> String -> String -> IO ()
gameLoop board bStrat wStrat bName wName = do
    putStrLn"Enter the move coordinates for player Black in the form 'srcX srcY destX destY' [0 >= n >= 4, or just enter return for a 'pass'] B2:\n"
    bMove <- bStrat board Normal Black
    putStrLn"Enter the move coordinates for player White in the form 'srcX srcY destX destY' [0 >= n >= 4, or just enter return for a 'pass'] W2:\n"
    wMove <- wStrat board Normal White
    let nextState = GameState (if bMove == Nothing 
                            then Passed 
                            else if(validateMove (theBoard board) (fromJust bMove) Black)
                            then Played (head (fromJust bMove), head (tail (fromJust bMove)))
                            else Goofed (head (fromJust bMove), head (tail (fromJust bMove))))
                            (blackPen board + if(bMove /= Nothing
                            && validateMove (theBoard board) (fromJust bMove) Black == False) then 1 else 0)
                            (if wMove == Nothing 
                            then Passed 
                            else if(validateMove (theBoard board) (fromJust wMove) White)
                            then Played (head (fromJust wMove), head (tail (fromJust wMove)))
                            else Goofed (head (fromJust wMove), head (tail (fromJust wMove))))
                            (whitePen board + if(wMove /= Nothing
                            && validateMove (theBoard board) (fromJust wMove) White == False) then 1 else 0)
                            (changeBoard (theBoard board) bMove wMove)
    putStrLn (show nextState)
    if (isGameOver nextState bMove wMove) == True then gameOver nextState bName wName
      else
        if checkEnd nextState
          then placer nextState bStrat wStrat bName wName
          else gameLoop nextState bStrat wStrat bName wName
          
-- |Edits the [[a]] list representing the board to show the change that the user's inputted move
--  has done to the board. Most of the game's piece movement logic is in here.
changeBoard :: Board -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> Board
changeBoard board bMove wMove
    |bMove == Nothing && wMove == Nothing = board
    |bMove == Nothing && wMove /= Nothing = do
        if (validateMove board (fromJust wMove) White) then(replace2 (replace2 (board)((fromJust wMove) !! 1)(getFromBoard (board) ((fromJust wMove) !! 0)))((fromJust wMove) !! 0)E)
        else board
    |bMove /= Nothing && wMove == Nothing = do
        if (validateMove board (fromJust bMove) Black) then(replace2 (replace2 (board)((fromJust bMove) !! 1)(getFromBoard (board) ((fromJust bMove) !! 0)))((fromJust bMove) !! 0)E)
        else board
    |otherwise = do
        let wPiece = getFromBoard (board) ((fromJust wMove) !! 0)
        let bPiece = getFromBoard (board) ((fromJust bMove) !! 0)
        if(((fromJust wMove) !! 1)==((fromJust bMove) !! 1) && (validateMove board (fromJust bMove) Black) && (validateMove board (fromJust wMove) White))
        then replace2 (replace2 (replace2 board ((fromJust wMove) !! 0) E) ((fromJust bMove) !! 0) E) ((fromJust bMove) !! 1) (handleClash bPiece wPiece)
        else if(validateMove board (fromJust bMove) Black)
        then(if(validateMove board (fromJust wMove) White)--both moves are valid
               then(replace2 (replace2 (replace2 (replace2 board ((fromJust wMove) !! 0) E) ((fromJust bMove) !! 0) E) ((fromJust bMove) !! 1) bPiece) ((fromJust wMove) !! 1) wPiece)
               else(replace2 ((replace2 board ((fromJust bMove) !! 0) E)) ((fromJust bMove) !! 1) bPiece)) --only bMove is valid
        else(if(validateMove board (fromJust wMove) White)--only wMove is valid
                then(replace2 ((replace2 board ((fromJust wMove) !! 0) E)) ((fromJust wMove) !! 1) wPiece)
                else(board)) --niether moves are valid
    
-- |Logic that handles if a clash has happened on the board, takes in the two clashing pieces
--  returns what the victorious piece is
handleClash :: Cell -> Cell -> Cell
handleClash bPiece wPiece
    |wPiece==WP && bPiece==BP = E
    |wPiece==WP && bPiece==BK = BK
    |wPiece==WK && bPiece==BP = WK
    |wPiece==WK && bPiece==BK = E
    |otherwise = E
    
-- |Function that checks the current GameState and Moves inputted to see if the game should end
isGameOver :: GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> Bool
isGameOver board bMove wMove
    | bMove == Nothing && wMove == Nothing = True
    | elem '/' (board2Str (theBoard board)) == False = True
    | elem '+' (board2Str (theBoard board)) == False = True
    | (blackPen board) >= 2 || (whitePen board) >= 2 = True
    | otherwise = False
    
-- |IO that is run when the game actually finishes, prints out the winner.
gameOver :: GameState -> String -> String -> IO() 
gameOver board bName wName
              | [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))] > [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] = putStrLn ("White Wins! " ++ "  Black ("++bName++"): " ++ [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] ++ " White ("++wName++"): " ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])
              | [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))] < [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] = putStrLn ("Black Wins! " ++ "  Black ("++bName++"): " ++ [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] ++ " White ("++wName++"): " ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])
              | [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))] == [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] = putStrLn ("DRAW! " ++ "  Black ("++bName++"): " ++ [intToDigit(length (findIndices (== '+') (board2Str (theBoard board))))] ++ " White ("++wName++"): " ++ [intToDigit(length (findIndices (== '/') (board2Str (theBoard board))))])

-- |Simple function to check if a pawn has reached the opposite side of the board, qualifying a promotion
checkEnd :: GameState -> Bool
checkEnd board = if elem BP (head(theBoard board)) || elem WP (last(theBoard board)) 
        then True
        else False
        
-- |Function to promote a pawn to a Knight, called upon when the number of ally Knights < 2
promotion :: Board -> Player -> Maybe[(Int,Int)] -> Board
promotion board Black move = if move == Nothing && (length(findIndices (== '#') (board2Str board))) < 2 then replace2 board ((findPawn (head board) BP),0) BK
                              else if move /= Nothing then replace2 (replace2 board ((fromJust move) !! 0) BP) ((findPawn (head board) BP),0) E
                                else board
promotion board White move = if move == Nothing && (length(findIndices (== 'X') (board2Str board))) < 2 then replace2 board ((findPawn (last board) WP),4) WK
                              else if move /= Nothing then replace2 (replace2 board ((fromJust move) !! 0) WP) ((findPawn (last board) WP),4) E
                                else board

-- |Logic that handles placing a pawn at a specified location, called upon when a pawn has successfully reached the other
--  side of the board
placer :: GameState -> Chooser -> Chooser -> String -> String -> IO()
placer board bStrat wStrat bName wName = do
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
                                    else promotion (theBoard board) Black bMove 
                                  else if w && isValid wMove board
                                    then  promotion (theBoard board) White wMove 
                                    else (theBoard board))
    putStrLn (show nextState)
    gameLoop nextState bStrat wStrat bName wName

-- |Simple function that counts how many pawns there are left
findPawn :: [Cell] -> Cell -> Int
findPawn (x:xs) c = if x == c 
                      then 0
                      else 1 + findPawn xs c 
                      
-- |Simple function to check if the spot you are moving to is empty
isValid :: Maybe[(Int,Int)] -> GameState -> Bool
isValid move board 
              | move == Nothing = True
              | (getFromBoard (theBoard board) ((fromJust move) !! 0)) == E = True
              | otherwise = False
              
-- |Simple function to check if two moves are moving to the same tile, would cause a clash
isMoveEqual :: Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> Bool
isMoveEqual move1 move2 
              | move1 == Nothing && move2 == Nothing = False
              | (head(fromJust move1)) == (head(fromJust move2)) = True
              | otherwise = False
    
-- |takes in the board, list of tuples indicating the move, Player("Black" or "White")
--  and checks to see if the move is valid
validateMove :: Board -> [(Int,Int)] -> Player -> Bool
validateMove board [(x1,y1),(x2,y2)] Black --for black pieces
    |getFromBoard board (x1,y1) == BP && x1==x2 && y1-1==y2 
        && getFromBoard board (x2,y2)== E = True
    |(getFromBoard board (x1,y1) == BP && x1+1==x2 && y1-1==y2 
        && (getFromBoard board (x2,y2)== WK
        || getFromBoard board (x2,y2)== WP)) = True
    |(getFromBoard board (x1,y1) == BP && x1-1==x2 && y1-1==y2
        && (getFromBoard board (x2,y2)== WK
        || getFromBoard board (x2,y2)== WP)) = True
    |getFromBoard board (x1,y1) == BK && x1+1 == x2 && y1+2 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1-1 == x2 && y1+2 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1+1 == x2 && y1-2 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1-1 == x2 && y1-2 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1+2 == x2 && y1+1 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1-2 == x2 && y1+1 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1+2 == x2 && y1-1 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    |getFromBoard board (x1,y1) == BK && x1-2 == x2 && y1-1 == y2
        && (getFromBoard board (x2, y2) /= BK
        && getFromBoard board (x2, y2) /= BP) = True
    | otherwise = False
validateMove board [(x1,y1),(x2,y2)] White --for white pieces
    |getFromBoard board (x1,y1) == WP && x1==x2 && y1+1==y2 
        && getFromBoard board (x2,y2)== E = True
    |(getFromBoard board (x1,y1) == WP && x1+1==x2 && y1+1==y2 
        && (getFromBoard board (x2,y2)== BK
        || getFromBoard board (x2,y2)== BP)) = True
    |(getFromBoard board (x1,y1) == WP && x1-1==x2 && y1+1==y2
        && (getFromBoard board (x2,y2)== BK
        || getFromBoard board (x2,y2)== BP)) = True
    |getFromBoard board (x1,y1) == WK && x1+1 == x2 && y1+2 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1-1 == x2 && y1+2 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1+1 == x2 && y1-2 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1-1 == x2 && y1-2 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1+2 == x2 && y1+1 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1-2 == x2 && y1+1 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1+2 == x2 && y1-1 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    |getFromBoard board (x1,y1) == WK && x1-2 == x2 && y1-1 == y2
        && (getFromBoard board (x2, y2) /= WK
        && getFromBoard board (x2, y2) /= WP) = True
    | otherwise = False
    