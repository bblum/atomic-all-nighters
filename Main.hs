-- Main routines for Atomic All-Nighters
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Main where

import Control.Monad (when)
import Language.C
import Language.C.System.GCC
import System.Exit

import Check

micro_name = "ATOMIC_ALL_NIGHTERS"

parseFile :: FilePath -> IO CTranslUnit
parseFile input_file =
    do parse_result <- parseCFile (newGCC "gcc") Nothing ["-D" ++ micro_name] input_file
       case parse_result of
           Left parse_err -> error (show parse_err)
           Right ast      -> return ast

-- TODO: cmdline options
main :: IO ()
main =
    do ast <- parseFile "onion-station.c"
       print $ pretty ast
       let msgs = check ast
       mapM_ putStrLn msgs
       when (not $ null msgs) $ exitWith $ ExitFailure 1

