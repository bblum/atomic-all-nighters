-- Main routines for Atomic All-Nighters
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Main where

import Language.C
import Language.C.System.GCC

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
    do ast <- parseFile "noob.c"
       print $ pretty ast
