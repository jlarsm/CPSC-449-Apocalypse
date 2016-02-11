{- | This module is used for CPSC 449 for the Apocalypse assignment.

This is merely a skeleton to get you started.  It has VERY little functionality.

Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module ApocStrategyHuman where

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
human b PawnPlacement c = return (Just [(2,2)])
mover :: String -> IO (Maybe [(Int,Int)])
mover xs = 
    if xs == "" then
        return Nothing
    else do 
        let x = [digitToInt x | x <- xs, isDigit x]; 
        if length x < 4 then do putStrLn "Invalid"; d <- getLine; mover d
        else do
            return(Just (zip (head x : x !! 2 : []) (x !! 1 : x !! 3 : [])))
