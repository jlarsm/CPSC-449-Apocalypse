{-|
Module      : CmdLineArgs
Description : Library supporting processing command line arguments
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

This module supports command line argument parsing by allowing the user to specify
the command line through the 'ParamSpec' structure.  It also supports writing a
"synopsis" to Stdout using the same structure (see 'printSynopsis').
-}
module CmdLineArgs (
  -- * Types
  ParamStat(ValRequired,ValRequiredIfPresent,Present,Absent),
  ParamSpec(ParamSpec,pName,pStat,pVal,pHelp),
  -- * Functions
  -- ** Parsing
  initFromArgs,
  -- ** Information
  getParam,
  isQualSpecified,
  isQualPresent,
  getQual,
  -- ** Help Support
  printSynopsis
  ) where

import System.Environment
import System.Exit
import Data.List
import Data.Maybe
import Control.Exception

{- | The "status" of a 'ParamSpec'.
-}
data ParamStat = ValRequired          -- ^ Parameter or qualifier must be present and have a value (usually only for a parameter).
               | ValRequiredIfPresent -- ^ If the param or qual is present, it must have a value.
               | Present              -- ^ Item is present.
               | Absent               -- ^ Item is absent.
                 deriving (Eq, Show)

{- | Pass a list of 'ParamSpec's to 'initFromArgs' to parse the command line and update
     this list so that it can be used with informational functions, 'isParamPresent',
     'printSynopsis', 'getParam', and 'isQualSpecified'.  Parameters are specified by
     an empty-String 'pName', which must all occur first and required parameters must
     appear before optional ones.  For qualifiers, an empty-String 'pVal' specifies that
     the qualifier cannot take a value.

     When parsing all qualifiers are prefixed with a "-" otherwise a word is considered
     either a parameter or a value for the previous qualifier.  Use "--" (two minus signs)
     to turn off a default-on qualifier.

   @
     [ (ParamSpec ""     ValRequired ""   "Help text") -- A required parameter with no default value
     , (ParamSpec ""     Absent      ""   "Help text") -- Am optional parameter with no default value
     , (ParamSpec ""     Absent      "15" "Help text") -- An optional parameter with a default value
     , (ParamSpec "v"    Absent      ""   "Help text") -- An optional qualifier disallowing a value
     , (ParamSpec "mode" Absent      "r"  "Help text") -- An optional qualifier with a default value
     , (ParamSpec "dpth" ValueRequiredIfPresent "3" "Help text") -- An optional qualifier requiring a value
     , (ParamSpec "d"    ValRequired ""  "Help text") -- An required qualifier requiring a value
     ]
   @
-}
data ParamSpec = ParamSpec { pName::String    -- ^ The name of the parameter (without "-" in front of it.
                           , pStat::ParamStat -- ^ The status (see 'ParamStat').
                           , pVal::String     -- ^ The String value of the parameter, prefill with the default, use "" for no default
                           , pHelp::String    -- ^ The help String for the parameter
                           } deriving (Eq, Show)

lookupPName :: String -> [ParamSpec] -> Maybe ParamSpec
lookupPName name [] = Nothing
lookupPName name (p:ps) = if name==pName p then Just p else lookupPName name ps

getPNames :: [ParamSpec] -> [String]
getPName []      = []
getPNames (p:ps) = (pName p):getPNames ps

{- | Will exit the process if we find an argument that doesn't occur in @defaults@.
-}
initFromArgs :: [ParamSpec] -> IO [ParamSpec]
initFromArgs defaults = do args <- getArgs
                           --_ <- checkNoErrors (fst (unzip defaults)) args -- may exit the process
                           res <- initFromArgs' defaults 0 args
                           if isHelpSpecified res then return res else checkParams res

initFromArgs' :: [ParamSpec] -> Int -> [String] -> IO [ParamSpec]
initFromArgs' params _ [] = return params
initFromArgs' params n (('-':'-':arg):args) =  -- arg without value, turning the qual off
    let lu = lookupPName arg params
     in if lu==Nothing
        then do noSuchArg arg params
                return []
        else let i = fromJust lu
                 split = break (==i) params
                 newI = ParamSpec arg Absent (pVal i) (pHelp i)
                 ret = ((fst split) ++ [newI] ++ (if snd split == [] then [] else tail (snd split)))
              in initFromArgs' ret n args
initFromArgs' params n (('-':arg):(minus:next):args) | minus /= '-' = do -- arg with value
    let nxt = minus:next
        lu = lookupPName arg params
     in if lu==Nothing
        then do noSuchArg arg params
                return []
        else let i = fromJust lu
                 split = break (==i) params
                 newI = ParamSpec arg Present nxt (pHelp i)
                 ret = ((fst split) ++ [newI] ++ (if snd split == [] then [] else tail (snd split)))
              in initFromArgs' ret n args
initFromArgs' params n (('-':arg):args) =  -- arg without value
    let lu = lookupPName arg params
     in if lu==Nothing
        then do noSuchArg arg params
                return []
        else let i = fromJust lu
                 split = break (==i) params
                 newI = ParamSpec arg Present (pVal i) (pHelp i)
                 ret = ((fst split) ++ [newI] ++ (if snd split == [] then [] else tail (snd split)))
              in if pStat i == ValRequiredIfPresent
                 then do errValRequired arg params
                         return []
                 else initFromArgs' ret n args
initFromArgs' params n (arg:args) = do -- a parameter
    if n >= nParams params
    then do tooManyParams (n+1) params
            return []
    else
        let
            split = splitAt n params
            newI = ParamSpec "" Present arg (pHelp (params!!n))
            ret = ((fst split) ++ [newI] ++ (if snd split == [] then [] else tail (snd split)))
         in initFromArgs' ret (n+1) args

checkParams :: [ParamSpec] -> IO [ParamSpec]
checkParams ((ParamSpec "" ValRequired val _):params) = do -- doesn't return
    tooFewParams params
    return []
checkParams ((ParamSpec "" Present val help):params) = do -- keep trying
    checkParams params
    return ((ParamSpec "" Present val help):params)
checkParams x = return [] -- we're done



nParams :: [ParamSpec] -> Int
nParams ((ParamSpec "" _ _ _):params) = 1 + nParams params
nParams params = 0

{- | Will exit the process if we find an argument that doesn't occur in @defaults@.
-}
errValRequired :: String -> [ParamSpec] -> IO ()
errValRequired arg params = do
    err arg params "Value required for argument -"

{- | Will exit the process if we find an argument that doesn't occur in @defaults@.
-}
noSuchArg :: String -> [ParamSpec] -> IO ()
noSuchArg arg params = do
    err arg params "Unrecognized argument -"

{- | Will exit the process if we find an argument that doesn't occur in @defaults@.
-}
tooManyParams :: Int -> [ParamSpec] -> IO ()
tooManyParams n params = do
    err (show n) params "Too many parameters at position "

{- | Will exit the process if we find an argument that doesn't occur in @defaults@.
-}
tooFewParams :: [ParamSpec] -> IO ()
tooFewParams params = do
    err "" params "Too few parameters"

{- | Will exit the process if we find an argument that doesn't occur in @defaults@.
-}
err :: String -> [ParamSpec] -> String -> IO ()
err arg params msg  = do
    progName <- getProgName
    putStrLn $ msg ++ arg ++ ".\n"
    printSynopsis params
    exitFailure

isHelpAvailable :: [ParamSpec] -> Maybe String
isHelpAvailable params =
    let names = getPNames params
     in if elem "h" names then Just "h"
        else if elem "help" names then Just "help"
        else if elem "?" names then Just "?"
        else Nothing

isHelpSpecified :: [ParamSpec] -> Bool
isHelpSpecified params = isQualSpecified params ["h","help","?"]

{- | Returns True iff there is specification for one of the listed strings in
     one of the ParamSpecs.  NOT that it's set in the comment line.
-}
isQualSpecified :: [ParamSpec] -> [String] -> Bool
isQualSpecified params [] = False
isQualSpecified params (x:xs) =
    if elem x (getPNames params) then True else isQualSpecified params xs

{- | Returns True iff the qualifier is present on the command line.
-}
isQualPresent :: String -> [ParamSpec] -> Bool
isQualPresent p spec =
    let v = lookupPName p spec
     in if v == Nothing
        then False
        else elem (pStat (fromJust v)) [Present,ValRequired]

{- | Retrieves the value of the nth parameter.  The index of the first parameter is 0.
     If there are not at least n+1 parameters then returns "".
-}
getParam :: Int -> [ParamSpec] -> String
getParam n params | n>=(length params) || pName (params!!n) /= "" = ""
getParam n params =
     let v = params !! n
     in  pVal v

{- | Retrieves the value the qualifier. If the qualifier is absent from the command
   line or does not have a value, returns the empty string.
-}
getQual :: String -> [ParamSpec] -> String
getQual q params =
    let v = lookupPName q params
     in if v == Nothing
        then ""
        else pVal (fromJust v)

{- | Prints a synopsis of the command line.
-}
printSynopsis :: [ParamSpec] -> IO ()
printSynopsis pss = do
    name <- getProgName
    putStrLn $ "Synopsis:\n\n  " ++ name ++ formatParamsSyn 0 pss ++ " [quals]"
    printQuals pss 0

{- | Format the parameter description for the command line display showing each of
   the parameters with brackets if they are optional.
-}
formatParamsSyn :: Int -> [ParamSpec] -> String
formatParamsSyn index ps | index >= length ps || pName (ps!!index) /= "" = ""
formatParamsSyn index ps = let
       optional = pStat (ps!!index) /= ValRequired || pVal (ps!!index) /= ""
    in (if optional then " [p" else " p") ++
       show index ++
       (if optional then "]" else "") ++
       formatParamsSyn (index+1) ps

{- | Prints out the parameters and qualifier that are specified in the'ParamSpec'.
-}
printQuals :: [ParamSpec] -> Int -> IO ()
printQuals [] n = do putStrLn ""
printQuals (ps:pss) n = do
    if n==0 && (pName ps=="")
    then do putStrLn "\nParameters: "
            formatQual ps n
            printQuals pss (n+1)
    else if (pName ps=="")
         then do formatQual ps n
                 printQuals pss (n+1)
         else if (n>=0)
              then do putStrLn "\nQualifiers:"
                      formatQual ps (-n)
                      printQuals pss (-1-n)
              else do formatQual ps n
                      printQuals pss (n-1)

{- | Formats a 'ParamSpec' for printing.
-}
formatQual :: ParamSpec -> Int -> IO ()
formatQual ps n =
  let hd = (if pName ps==""
            then "  p"++(show n)++""
            else "  -" ++ (pName ps)
                 ++ (if elem (pStat ps) [ValRequired,ValRequiredIfPresent] then " val" else ""))
   in do putStr $ hd ++ (take (15 - length hd) (repeat ' ')) ++ ": " ++ (pHelp ps) ++ "."
         if (pVal ps)/=""
         then putStrLn $ " (def=" ++ (pVal ps) ++ ")"
         else putStrLn ""
