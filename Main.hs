-- Main routines for Atomic All-Nighters
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Main where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Language.C
import Language.C.System.GCC
import System.Environment (getArgs)
import System.Exit
import System.Console.GetOpt

import Check

--
-- Options & Usage
--

data Options = Options { help :: Bool, verbose :: Bool }
defaultOptions = Options { help = False, verbose = False }

desc = [ Option ['h'] ["help"] (NoArg (\o -> o { help = True }))
         "Show this help text"
       , Option ['v'] ["verbose"] (NoArg (\o -> o { verbose = True }))
         "Show info messages in addition to warnings and errors"
       ]

header = "Atomic All-Nighters - static C code context checking\n" ++
         "Usage: aaa [OPTION...] SOURCEFILE"
helptext = usageInfo header desc

parseArgs :: [String] -> Either [String] (Options, String)
parseArgs a =
    case getOpt Permute desc a of
        (opts, [file], []) ->
            case foldl (flip id) defaultOptions opts of
                Options { help = True } -> Left [helptext]
                opts -> Right (opts, file) -- TODO: support mult. files
        (_, _, []) -> Left [helptext]
        (_, _, errs) -> Left errs

--
-- Reading C files
--

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
    do x <- parseArgs <$> getArgs
       case x of
           Left errs ->
               do mapM_ putStrLn errs
                  exitWith $ ExitFailure 1
           Right (opts, file) ->
               do ast <- parseFile file
                  print $ pretty ast
                  let msgs = check ast
                  mapM_ putStrLn msgs
                  when (not $ null msgs) $ exitWith $ ExitFailure 1

