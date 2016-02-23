{- |
Module      : ApocTools
Description : Required definitions for the CPSC 449 W2016 Haskell Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3


This module is used for CPSC 449 for the Apocalypse assignment.

You MUST use this file as part of your assignment to deal with boards,
cells, etc.  This may be tested by linking your assignment against a modified
version of this file.

Do not modify this file.

-}

-- The following pragmas are only necessary for the Read class instance of 'GameBoard'
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ApocTools (
    -- * Cell (A "square" on the board)
    Cell(WK,WP,BK,BP,E),
    cell2Char,
    char2Cell,
    putCell,
    -- * The board itself
    Board,
    initBoard,
    putBoard,
    board2Str,
    getFromBoard,
    -- * Players and pieces
    Player(Black,White),
    Piece(BlackKnight,BlackPawn,WhiteKnight,WhitePawn),
    pieceOf,
    playerOf,
    -- * Move descriptions
    Played(Played,Passed,Goofed,Init,UpgradedPawn2Knight,PlacedPawn,BadPlacedPawn,NullPlacedPawn,None),
    PlayType(Normal,PawnPlacement),
    -- * The game state
    GameState(GameState,blackPlay,blackPen,whitePlay,whitePen,theBoard),
    -- * The interface for a strategy
    Chooser

    ) where

import Data.Char (isSpace)

---Cells-----------------------------------------------------------
---Cells are the state of a cell: contains White and Black Pawns and Knights or is Empty

{- | The possible contents of a cell: 'WK', 'BK', 'WP', 'BP', and Empty ('E').
     We do NOT include "deriving (Show)" here because we use the "instance Show ..."
     so we can customize it's display from (say) "Cell E" to "_" according to the
     'cell2Char' function.
-}
data Cell = WK   -- ^ White knight
          | WP   -- ^ White pawn
          | BK   -- ^ Black knight
          | BP   -- ^ Black pawn
          | E   -- ^ Empty
          deriving (Eq) --deriving (Show)

-- | Customized print form of Cell
instance {-# OVERLAPS #-}
         Show (Cell) where
         show c = [cell2Char c]

-- | Converts a 'Cell' to a displayable Char
cell2Char       :: Cell -> Char
cell2Char WK    =  'X'
cell2Char WP    =  '/'
cell2Char BK    =  '#'
cell2Char BP    =  '+'
cell2Char E     =  '_'

-- | Converts a 'Char' to a 'Cell'
char2Cell       :: Char -> Cell
char2Cell 'X'   = WK
char2Cell '/'   = WP
char2Cell '#'   = BK
char2Cell '+'   = BP
char2Cell '_'   = E

{- | IO function to print a 'Cell' in a 'Board', which is printed as '|' and the char
     representation of the 'Cell'.
-}
putCell         :: Cell -> IO()
putCell c       = do putChar '|'
                     putChar (cell2Char c)

---Boards--------------------------------------------------------
---A board is just a 2d (8x8) list of Cells

-- | The representation of the 'Board' (which is 5x5).
type Board      = [[Cell]]

-- | Customize the read function for 'Board' to coorespond with the show function.
instance {-# OVERLAPS #-}
         Read Board where
         readsPrec _ r = [(result, remainder)]
                             where
                             allLines = lines (dropWhile (isSpace) r)
                             lss = take 6 allLines
                             ls = tail lss
                             rows = map (filter (/='|')) ls
                             result = map (map char2Cell) rows
                             remainder = unlines $ drop 6 allLines

-- | Customized Show for 'Board' so it's more human-readable
instance {-# OVERLAPS #-} Show Board where show b = board2Str b

-- | The intial state of the board
initBoard       :: GameState
initBoard       = GameState Init 0 Init 0
                  [ [WK, WP, WP, WP, WK],
                    [WP, E , E , E , WP],
                    [E , E , E , E , E ],
                    [BP, E , E , E , BP],
                    [BK, BP, BP, BP, BK] ]

-- | Print out a row in a 'Board' in the for "|?|?|?|?|?|".
putRow          :: [Cell] -> IO()
putRow r        = do mapM_ putCell r
                     putStr "|\n"

-- | return a row in a 'Board' in the for "|?|?|?|?|?|".
row2Str        :: [Cell] -> String
row2Str []     = []
row2Str (x:xs) = "|" ++ show x ++ row2Str xs

{- | IO function to print out a 'Board' in the form of:

@
 _ _ _ _ _
|?|?|?|?|?|
|?|?|?|?|?|
|?|?|?|?|?|
|?|?|?|?|?|
|?|?|?|?|?|
@
Where the question marks are replaced with the appropriate 'Cell' character (see
'cell2Char').
-}
putBoard        :: [[Cell]] -> IO()
putBoard a      = do
                    putStr " _ _ _ _ _\n"
                    mapM_ putRow a
                    putStr ""

-- | Return a string representation of a 'Board' in the same form as 'putBoard', above.
board2Str         :: [[Cell]] -> String
board2Str b       = " _ _ _ _ _\n" ++ board2Str' b
-- | Helper function for 'board2Str'.
board2Str'        :: [[Cell]] -> String
board2Str' []     = []
board2Str' (x:xs) = row2Str x ++ "|\n" ++ board2Str' xs

-- | Return the 'Cell' at a point from a 'Board'.
getFromBoard             :: [[a]] -> (Int,Int) -> a
getFromBoard xs pt       = xs !! snd pt !! fst pt

---Game state-------------------------------------------------------

-- | Represents a Player (Black or White).
data Player    = Black | White deriving (Eq, Show, Read)

-- | Represents a Piece, which is slightly different from 'Cell' as a player can't be empty.
data Piece     = BlackKnight | BlackPawn | WhiteKnight | WhitePawn deriving (Eq, Show, Read)

-- | Given a 'Cell', return the corresponding 'Piece'.
pieceOf        :: Cell -> Piece
pieceOf  BK     = BlackKnight
pieceOf  BP     = BlackPawn
pieceOf  WK     = WhiteKnight
pieceOf  WP     = WhitePawn

-- | Given a 'Piece', return the corresponding 'Player'.
playerOf            :: Piece -> Player
playerOf BlackKnight = Black
playerOf BlackPawn   = Black
playerOf WhiteKnight = White
playerOf WhitePawn   = White

-- | Represents the type of move played in a 'GameState'.
data Played = Played ((Int, Int), (Int, Int)) -- ^ A "normal" move.
            | Passed                          -- ^ A (legal) pass.
            | Goofed ((Int, Int), (Int, Int)) -- ^ An illegal move, penalty applied.
            | Init                            -- ^ No one has moved yet.
            | UpgradedPawn2Knight (Int,Int)   -- ^ A pawn reached the other side when <2 knights.
            | PlacedPawn ((Int, Int), (Int, Int)) -- ^ A pawn that's been placed in any empty space after having reached the far end of the board.
            | BadPlacedPawn ((Int, Int), (Int, Int)) -- ^ A strategy has attempted to do a pawn placement, but chose an invalid location.
            | NullPlacedPawn -- ^ A strategy has attempted to do a pawn placement, but returned Nothing
            | None -- ^ the legitimate 'pass' when the other player does a PlacedPawn
              deriving (Eq, Show, Read)

{- | Represents the current state of the game.  Contains:

     * what each Player Played
     * their penalties
     * the state of the board.
-}
data GameState = GameState { blackPlay :: Played  -- ^ The black player's play type
                           , blackPen  :: Int     -- ^ The black player's penalty
                           , whitePlay :: Played  -- ^ The white player's play type
                           , whitePen  :: Int     -- ^ The white player's penalty
                           , theBoard  :: Board   -- ^ The actual board.
                           } deriving (Eq)

-- | Customize the print form of 'GameState'.
instance Show (GameState) where
         show g = ">>>\n"
                  ++ "(" ++ show (blackPlay g) ++ ", " ++ show (blackPen  g) ++ ")\n"
                  ++ "(" ++ show (whitePlay g) ++ ", " ++ show (whitePen  g) ++ ")\n"
                  ++ show (theBoard g)


-- | Customize the read function for 'Board' to coorespond with the show function.
instance Read GameState where
    readsPrec _ r =
      case readsPrec 0 (dropWhile (isSpace) (scanPastFlag r)) :: [((Played,Int),String)] of
        (((bPlay,bPen),rest1):_) ->
          case readsPrec 0 (dropWhile (isSpace) (tail rest1)) :: [((Played,Int),String)] of
            (((wPlay,wPen),rest3):_) ->
              case readsPrec 0 (tail rest3) :: [(Board,String)] of
                ((board,rest5):_) ->
                  [(GameState bPlay bPen wPlay wPen board, rest5)]
                e3 -> []
            e2 -> []
        e1 -> []

scanPastFlag :: String -> String
scanPastFlag []                    = ""
scanPastFlag ('>':'>':'>':'\n':cs) = cs
scanPastFlag (c:cs)                = scanPastFlag cs

-- | The text version of a state for testing purposes.
testState = "some garbage\n\
            \>>>\n\
            \(PlacedPawn ((0,0),(3,2)), 1)\n\
            \(None, 0)\n\
            \ _ _ _ _ _\n\
            \|_|_|_|_|X|\n\
            \|_|_|_|_|/|\n\
            \|_|_|_|+|_|\n\
            \|+|_|_|_|+|\n\
            \|#|_|+|+|#|"

---Strategies-------------------------------------------------------

{- | This type is used by 'Chooser' to tell the 'Chooser' strategy to generate either a
     'Normal' move (source and destination) or a 'PawnPlacement' move (just a destination).
-}
data PlayType = Normal -- ^ The 'Chooser' should return a list of 2 (x,y) coordinates.
              | PawnPlacement -- ^ The 'Chooser' should return a singleton list of (x,y) coordinates.
               deriving Eq

{- | This is the type for all player functions.  A player strategy function takes

    1. a 'GameState' on which to base it's decision
    2. 'PlayType', which may be 'Normal' to indicate that it must return both a source
       and destination coordinate in the form [(a,b),(c,d)] where the letters must be
       integers; or it may be 'PawnPlacement' to indicate that it must return just a
       singlton list containing an empty cell in the 'Board'.
    3. 'Player' which indicates that the strategy must work from the perspective of
       'Black' or 'White'.

    The return value can will be Just [(Int,Int)] with either one or two elements (see
    point 2 above), or it may return Nothing to indicate a "pass".
-}
type Chooser = GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])


