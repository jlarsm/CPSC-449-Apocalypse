{-|
Module      :   HumanStrat
Description :   IO input, not an actual strategy. Simply prompts user for an input whenever
                it is this chooser/strategies' turn.
Stability   :   experimental
Portability :   ghc 7.10.2 - 7.10.3
-}

module HumanStrat where

import System.Random
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import Data.List
import Data.Char
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human b Normal        c = do d <- getLine; mover d
human b PawnPlacement c = do putStrLn ("Enter the coordinates to place the pawn for player"++(player2String c)++"in the form 'destX destY':\n[0 >= n >= 4]"++(player2String' c)++"1:\n"); d <- getLine; pawnPlacer d
mover :: String -> IO (Maybe [(Int,Int)])
mover xs = 
    if xs == "" then
        return Nothing
    else do 
        let x = [digitToInt x | x <- xs, isDigit x]; 
        if length x < 4 then do putStrLn "Invalid Input: Input must be 4 digits in length and have format 'srcX srcY destX destY' 0 >= n >= 4"; d <- getLine; mover d
        else 
            if ((head x) < 0 || (head x) > 4 || (x !! 1) < 0 || (x !! 1) > 4 || (x !! 2) < 0 || (x !! 2) > 4 || (x !! 3) < 0)
                then do putStrLn "Invalid Input: Out Of Range (0 <= x <= 4)"; d <- getLine; mover d
                else do return(Just (zip (head x : x !! 2 : []) (x !! 1 : x !! 3 : [])))
pawnPlacer :: String -> IO (Maybe [(Int,Int)])
pawnPlacer xs = if xs == "" then return Nothing
                else do
                    let x = [digitToInt x | x <- xs, isDigit x];
                    if length x < 2 then do putStrLn ("Please place pawn on an empty spot '_'"); str' <- getLine; pawnPlacer str'
                    else if ((head x) < 0 || (head x) > 4 || (x !! 1) < 0 || (x !! 1) > 4) 
                        then do putStrLn ("Invalid Input: Out Of Range (0 <= x <= 4)"); str' <- getLine; pawnPlacer str'
                        else do return(Just (zip (head x : []) (x !! 1 : [])))
player2String :: Player -> String
player2String player
                | player == Black = " Black "
                | player == White = " White "
player2String' :: Player -> String
player2String' player
                | player == Black = "B"
                | player == White = "W"
