{- |
Module      : ApocCheckerMini
Description : Checks the Apocalypse assignment for CPSC 449 W16; minimal version that won't give away the solution.
Copyright   : (c) Rob Kremer, 2016
License     : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
              Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2-3

Checks the Apocalypse assignment for CPSC 449 W16.  This version is for students
to try out their programs, and so does NOT include code for checking game rules, etc.
Instead, it just uses tests against pre-recorded game files.  The input passed to the
student program is a pre-defined file.

   1. compiles the .hs file given as a command line argument.
   2. runs the compiled program with no arguments and collects the strategy names.
   3. runs several games from file input and compares them to template games.
   4. prints a report of errors in the program and stats of the strategies.

The compiling and the runs of the compiled program are all done in subprocesses.

Synopsis:

  __ApocCheckerMini [progFile] [quals]__

Parameters:

  [@progFile@]   The Apocalypse program to test. (def=Apoc)

Qualifiers:

  [@-v@]          Verbose. Print out detailed information.
  [@-h@]          Print out this helpful information.
  [@-t timeout@]  Subprocess timeout in milliseconds. (def=1000)

Requires the following external files specifying user input, and expected output to be
in the default directory:

  * test2passes.in
  * test2goofs.in
  * testBWins.in
  * test2passes.out
  * test2goofs.out
  * testBWins.out
-}

module ApocCheckerMini
    ( main
      -- * Data structures
    , Status(Correct, IncorrectMove, IncorrectRule, AnalysisFailed, IllegalMoveAllowed,
             OutOfTurn, IncorrectGoofClassification, IllegalGameContinued, NullGame)
    , Game
    , GameOutcome(Winner, Erred, BadPass, Tie)
      -- * Basic running tests
    , runTests
      -- * Running subprocesses
    , getStrategies
    , playGame
      -- * Reporting
    , report
      -- ** Reporting utility funcitons
    , sideBySide
    , warning
      -- * Untility
    , isDataLine
    )where

{-# LANGUAGE NamedFieldPuns #-}

import System.Environment (getArgs,getProgName)
import System.Process
import Control.Exception
import System.Exit
import Data.Text.Lazy (split)
import Data.List (permutations,nub,sort,delete)
import Data.Maybe (fromJust)
import ApocTools
import System.IO.Unsafe
import System.Timeout
import Text.Read(readMaybe)
import CmdLineArgs

tests = ["test2passes","test2goofs","testBWins"]

{- | Main program.  Read in the command-line parameters and qualifiers.  If these include
   the -h qualifier, then print the help info and quit.  Otherwise call 'runTests'
   to do the analysis as explained in teh module level doc.
-}
main :: IO ()
main = do
    args <- initFromArgs
          [ (ParamSpec ""  ValRequired "Apoc" "The Apocalypse program to test")
          , (ParamSpec "v" Absent      ""     "Verbose. Print out detailed information")
          , (ParamSpec "h" Absent      ""     "Print out this helpful information")
          , (ParamSpec "t" ValRequiredIfPresent "1000" "Subprocess timeout in milliseconds")
          ]
    if isQualPresent "h" args
    then printSynopsis2 args
    else do runTests args analyseProg

-- | The possible errors a control program could make.
data Status =   Correct
              | IncorrectMove
              | IncorrectRule
              | AnalysisFailed
              | IllegalMoveAllowed
              | OutOfTurn
              | IncorrectGoofClassification
              | IllegalGameContinued
              | NullGame
                  deriving (Eq, Show)

-- | All the possible game outcomes.
data GameOutcome =   Winner Player
                   | Erred Player
                   | BadPass Player
                   | Tie

-- | All the possible game outcomes from a player's perspective.
data PlayerOutcome = PWinner | PLooser | PErred | PBadPass | PTie deriving (Eq)

-- | A simple shorthand for a list of GameState's.
type Game = [GameState]

{- | Called by main to actually do the work.  Parameters are
        1. __['ParamSpec']__: The command-line parameters as specified by the CmdLine module.
        2. __testScript__: The function to use for analysis.
-}
runTests :: [ParamSpec] -> (String -> Int -> Bool -> IO ()) -> IO ()
runTests args testScript = do
    prog <- compileFile (getParam 0 args)
    if prog == []
    then do putStrLn $ "Could not compile target program, '" ++ (getParam 0 args) ++ "'.\n"
            printSynopsis2 args
    else let timeout = case readMaybe (getQual "t" args) of
                         Nothing -> 1000000::Int
                         Just t  -> t*1000
          in testScript prog timeout (isQualPresent "v" args)

{- | Analyse the target (assignment) program.  In this case, we are going to compare
   a run of the program with input from several *.in files with the expected output in
   the corresponding *.out files (where the "*" are the names listed in 'tests'.
-}
analyseProg :: String -> Int -> Bool -> IO ()
analyseProg prog timeout verbose = do
        allStrategies <- (getStrategies prog timeout)
        strategies <- return $ delete "human" allStrategies -- don't run the "human" startegy
        hasHumanStrat <- return $ length allStrategies > length strategies
        if strategies == []
        then do putStrLn "Either the program incorrectly listed strategies or the program timed out."
                putStrLn "Try increasing the timeout with command line qualifier -t."
        else do
          if hasHumanStrat
          then do
            testStrings <- readTests verbose tests
            testGames <- mapM (\x->playGame prog ("human","human") timeout verbose (snd x)) testStrings -- play the test games (human x human)
            templateGames <- mapM (readOutFile verbose) tests
            testAnalysis <- mapM (\x-> analyseGameAgainst verbose (fst x) (snd x)) (zip testGames templateGames)
            report (replicate (length tests) ("human","human")) testAnalysis verbose
          else do
            putStrLn "No human strategy listed by program.  Unable to do analysis."

-- | Read all files in the parameter list into a list of file-name/file-content pairs.
readTests :: Bool -> [String] -> IO [(String, String)]
readTests verbose [] = return []
readTests verbose (filename:rest) = do
    putStr $ if verbose then "Reading game from program with input from "++filename++"\n" else ""
    str <- readFile (filename++".in")
    rest' <- readTests verbose rest
    return $ (filename, str):rest'

{- | Perform an analysis of a single game returning the final Status, a human-readable
     comment, the GameOutcome, and turn number in the game.
-}
analyseGameAgainst :: Bool -> Game -> Game -> IO (Status, String, GameOutcome, Int)
analyseGameAgainst verbose game template = do
   putStr $ if verbose then "Analysing game...\n" else ""
   putStr $ if length game /= length template then "Game and template don't match\n" else ""
   ret <- analyseGameAgainst' game template 1
   putStr $ if verbose then " done.\n" else ""
   return ret

{- | Helper function for 'analyseGameAgainst'. We need this because we add a parameter
   to take care of keeping track of the rounds.
-}
analyseGameAgainst' :: Game -> Game -> Int -> IO (Status, String, GameOutcome, Int)
analyseGameAgainst' [] _ 1
        = do putStrLn "Empty game. Likely cause: subprocess timeout.  Try increasing timeout using cmd line qualifier -t; or check for infinite looping."
             return (NullGame, "Program Timeoutout", Tie, 1)
analyseGameAgainst' (state:[]) (tstate:[]) turn
    | state == tstate
        = return (Correct, "OK", Tie, turn)
analyseGameAgainst' (state:[]) (tstate:tstate':[]) turn
        = return (IncorrectRule,
                  (warning "Game terminated prematurely" (Just state) (Just tstate))
                            ++(sideBySide Nothing (Just tstate')),
                  Tie, turn)
analyseGameAgainst' (state:state':[]) (tstate:[]) turn
        = return (IncorrectRule,
                  (warning "Game continued past termination:" (Just state) (Just tstate))
                            ++(sideBySide (Just state') Nothing),
                  Tie, turn)
analyseGameAgainst' (state:state':states) (tstate:tstate':tstates) turn
    | state == tstate && state' == tstate'
        = analyseGameAgainst' (state':states) (tstate':tstates) (turn+1)
    | state == tstate && state' /= tstate'
        = return (IncorrectMove,
                  warning "States don't match" (Just state') (Just tstate'),
                  Tie, turn)

{- | Format a warning message with 'sideBySide' gameBoards.
-}
warning :: String -> Maybe GameState -> Maybe GameState -> String
warning msg g1 g2 = msg ++ ":" ++ (sideBySide g1 g2) ++ "\n"

-- | Format two boards so that they are side-by-side for printing
sideBySide :: Maybe GameState -> Maybe GameState -> String
sideBySide g g' = let
    b  = if g ==Nothing then Nothing else Just $ theBoard $ fromJust g
    b' = if g'==Nothing then Nothing else Just $ theBoard $ fromJust g'
    in foldr (++) [] $ zipWith (\a -> \b->"\n"++(fillr a 20)++b)
                            (if b ==Nothing then take 10 $ repeat "           "
                                            else lines (show $ fromJust b ))
                            (if b'==Nothing then take 10 $ repeat ""
                                            else lines (show $ fromJust b'))

{- |Plays a game using two strategies and returns the results.
   Args:
     1. The name of the program to run.
     2. The names of the black and white strategies respectively.
     3. The length of time (in milliseconds) to let the target program run.
     4. True if we want verbose output.
     3. Either [] or stdin for the process.
     Returns a tuple of the game status and a string report of the results.
-}
playGame :: String -> (String, String) -> Int -> Bool -> String -> IO Game
playGame prog (black, white) theTimeout verbose stdin = do
    putStr $ if verbose then "-------------------------------------------------------------------------------\n" else ""
    state <- timeout theTimeout (readProcessWithExitCode ("./"++prog) [black, white] stdin)
    case state of
      Nothing -> return []
      Just (exitCode, stdout, stderr) -> do
                putStr $ if verbose then stdout++"\n" else ""
                filtered <- return (unlines (filter isDataLine (lines stdout)))
                readGame filtered

{- | Reads a file and returns a 'Game' (list of 'GameState's) parsed from the file. The
   Bool parameter is for verbose.
-}
readOutFile :: Bool -> String -> IO Game
readOutFile verbose fileName = let fn = fileName ++ ".out"
    in do putStr $ if verbose then "Reading file: " ++ fn ++ " ..." else ""
          buf <- readFile (fn)
          game <- readGame buf
          putStr $ if verbose then " done.\n" else ""
          return game

-- | Converts a String readout from the output of the control program to a Game.
readGame :: String -> IO Game
readGame s = do result <- catchAny (return (readsPrec 0 s :: [(GameState,String)])) (\e-> return [])
                case result of
                   [] -> return []
                   ((state, rest):_) -> do r <- readGame rest
                                           return $ state:r

-- | Utility function to catch any IO error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

-- | Returns True iff the param line is a data line (starts with ">>>", "(", " _", or "|").
isDataLine              :: String -> Bool
isDataLine ('>':'>':'>':cs) = True
isDataLine ('(':cs)     = True
isDataLine (' ':'_':cs) = True
isDataLine ('|':cs)     = True
isDataLine cs           = False

{- | Returns the strategies of the control program by running it in a subprocess with
     no arguments and reading the strategy list (those lines beginning with two spaces).
-}
getStrategies :: String -> Int -> IO [String]
getStrategies prog theTimeout = do
    state <- timeout theTimeout (readProcessWithExitCode ("./"++prog) [] [])
    case state of
      Just (exitCode, stdout, stderr) -> return $ map (drop 2) (filter isStrategy (lines stdout))
      Nothing -> return []

-- | Returns True iff this is a strategy on a line (it begins with two spaces).
isStrategy (' ':' ':cs) = True
isStrategy cs = False

-- | Prints a doc synopsis of this program's command line.
printSynopsis2 :: [ParamSpec] -> IO ()
printSynopsis2 args = do
  putStrLn
   "This program checks the Apocalypse assignment for CPSC 449 W16.\n\
   \1. compiles the .hs file given as a command line argument.\n\
   \2. runs the compiled program with no arguments and collects the strategy names.\n\
   \3. runs several games from file input and compares them to template games.\n\
   \4. prints a report of errors in the program and stats of the strategies.\n\
   \The compiling and the runs of the compiled program are all done in\n\
   \subprocesses.\n"
  printSynopsis args

{- | If the parameter has the ".hs" suffix, strip it off.
-}
getHProgFile :: [Char] -> [Char]
getHProgFile x =
  let rev = reverse x
      ext = reverse $ take 3 rev
      prog = reverse $ drop 3 rev
  in  if ext == ".hs"
      then prog
      else x

{- | Compiles the argument .hs file into an executable.
     It takes one argument, the name of the .hs file, and it MUST have the .hs in the name.
-}
compileFile :: String -> IO String
compileFile sourceFile =
  let prog = getHProgFile sourceFile
   in do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "ghc" [sourceFile] []
        putStrLn stdout
        putStr $ if (stdout=="") then "" else stdout ++ "\n"
        putStr $ if (stderr=="") then "" else "ERROR:\n"++ stderr ++ "\n"
        putStrLn $ if exitCode == ExitSuccess then "Compile Succeeded." else "COMPILE FAILED: "++ show exitCode
        if exitCode == ExitSuccess then return prog else return []

-----------------------------------------------------------------------------------
-- Report generating functions
-----------------------------------------------------------------------------------

{- | Main report. prints the report body (the results of each run.  And then a
     summary including the errors in the control program, and a table of strategy
     results.
-}
report :: [(String, String)] -> [(Status, String, GameOutcome, Int)] -> Bool -> IO ()
report perts analyses verbose = do
  putStrLn "==============================================================================="
  reportBody perts analyses
  reportSummary perts analyses verbose
  putStrLn "==============================================================================="

-- | Returns the text of report summary
reportSummary :: [(String, String)] -> [(Status, String, GameOutcome, Int)] -> Bool -> IO ()
reportSummary perts analyses verbose = do
  putStrLn $ (formatCount analyses Correct verbose) ++
             (formatCount analyses IncorrectMove verbose) ++
             (formatCount analyses IncorrectRule verbose) ++
             (formatCount analyses AnalysisFailed verbose) ++
             (formatCount analyses IllegalMoveAllowed verbose) ++
             (formatCount analyses OutOfTurn verbose) ++
             (formatCount analyses IncorrectGoofClassification verbose) ++
             (formatCount analyses IllegalGameContinued verbose) ++
             (formatCount analyses NullGame verbose) ++
             "--\n" ++
             "Strategy                  Won      Lost      Tied     Erred  Bad Pass\n"
  (strat0,_) <- return $ unzip perts
  strategies <- return $ sort (nub strat0)
  slines <- return $ map (formatStratLine (formatCountStratResults perts analyses)) strategies
  putStrLn $ unlines slines

-- | Returns the formatted count of the Status'es matching the 2nd parameter.
formatCount :: [(Status, String, GameOutcome, Int)] -> Status -> Bool -> String
formatCount analyses theStat verbose = let
  i = countStat analyses theStat
  in  if i==0 && not verbose then "" else (fill (show i) 4) ++ " " ++ show theStat ++ "\n"

-- | Returns the count of the Status'es matching the 2nd parameter.
countStat :: [(Status, String, GameOutcome, Int)] -> Status -> Int
countStat [] _ = 0
countStat ((stat,_,_,_):ss) theStat =
  (if stat==theStat then 1 else 0) + (countStat ss theStat)

-- | Returns a formatted line for the strategy table for the strategy in the 2nd parameter.
formatStratLine :: (String -> PlayerOutcome -> String) -> String -> String
formatStratLine f theStrat =
  (fillr theStrat 20) ++
  (fill (f theStrat PWinner) 10) ++
  (fill (f theStrat PLooser) 10) ++
  (fill (f theStrat PTie) 10) ++
  (fill (f theStrat PErred) 10) ++
  (fill (f theStrat PBadPass) 10)

{- | Return the formatted count of matches with the strategy in the 2nd param and the
     outcome in the 3rd param.
-}
formatCountStratResults :: [(String, String)] -> [(Status, String, GameOutcome, Int)] -> String -> PlayerOutcome -> String
formatCountStratResults strats analyses theStrat theRes = let
  i = countStratResults strats analyses theStrat theRes
  in
  (fill (show i) 2) ++ " "


{- | Return the count of matches with the strategy in the 2nd param and the
     outcome in the 3rd param.
-}
countStratResults :: [(String, String)] -> [(Status, String, GameOutcome, Int)] -> String -> PlayerOutcome -> Int
countStratResults _ [] _ _ = 0
countStratResults ((bStrat, wStrat):strats) ((Correct,_,outcome,_):stats) theStrat theRes =
  let focusStrat = if theStrat == bStrat
                   then Just Black
                   else if theStrat == wStrat
                        then Just White
                        else Nothing
  in
  case focusStrat of
    Nothing -> countStratResults strats stats theStrat theRes
    Just p  -> (case outcome of
                 Winner q  -> if bStrat==wStrat
                              then 0 -- don't count playing against oneself
                              else
                                if p == q && theRes == PWinner
                                then 1
                                else
                                  if p /= q && theRes == PLooser
                                  then 1
                                  else 0
                 Tie       -> if theRes == PTie             then 1 else 0
                 Erred q   -> if (p == q || bStrat == wStrat) && theRes == PErred   then 1 else 0
                 BadPass q -> if (p == q || bStrat == wStrat) && theRes == PBadPass then 1 else 0)
                + (countStratResults strats stats theStrat theRes)
countStratResults ((bStrat, wStrat):strats) ((_,_,outcome,_):stats) theStrat theRes =
  countStratResults strats stats theStrat theRes

-- | Print the formatted report body. This reports the errors made by the control program.
reportBody :: [(String, String)] -> [(Status, String, GameOutcome, Int)] -> IO ()
reportBody [] [] = do putStrLn "done"
reportBody (p:ps) (a:as) = do
  putStrLn $ reportline p a
  reportBody ps as

-- | return a formatted line for the report body for particular game.
reportline :: (String, String) -> (Status, String, GameOutcome, Int) -> String
reportline (strat1,strat2) (stat, msg, winner, turn) =
         (fill strat1 20) ++ " x " ++
         (fill strat2 20) ++ " (" ++
         (fill (outcome2Str winner) 10) ++ " on turn "++
         (fill (show turn) 2) ++ "): " ++ msg

-- | Left-fill the param string to pad it out so the result String has a length n.
fill :: String -> Int -> String
fill s n = concat [(take (n - (length s)) (repeat ' ')), s]

-- | Right-fill the param string to pad it out so the result String has a length n.
fillr :: String -> Int -> String
fillr s n = concat [s, (take (n - (length s)) (repeat ' '))]

-- | Convert a GameOutcome to a string for 'reportLine'
outcome2Str :: GameOutcome -> String
outcome2Str Tie             = "   tie      "
outcome2Str (Winner player) = "winner=" ++ show player
outcome2Str (Erred player)  = " error=" ++ show player

