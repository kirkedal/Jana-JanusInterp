
import System.Environment
import System.Exit
import System.Timeout
import Control.Monad
import Data.List
import Math.NumberTheory.Primes.Testing (isPrime)

-- import Jana.ParserBasic
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..), DebugMode(..), ModEval(..))
import Jana.Invert
import qualified Jana.JanusToC as JTC


data Options = Options
  { timeOut :: Int
  , invert :: Bool
  , ast :: Bool
  , cCode  :: Bool
  , header :: Maybe String
  , evalOpts :: EvalOptions }

defaults :: Options
defaults = Options
  { timeOut = -1
  , invert = False
  , ast = False
  , cCode = False
  , header = Nothing
  , evalOpts = defaultOptions }

usage :: String
usage = "usage: jana [options] <file>\n\
        \options:\n\
        \  -m[n]        use n-bit modular arithmetic; if (n) is unset 32 is used\n\
        \  -p[n]        use GF(n) finite field arithmetic; if (n) is unset M_31 (2147483647) is used\n\
        \  -tN          timeout after N seconds\n\
        \  -i           print inverted program\n\
        \  -c           print C++ program\n\
        \  -a           print program AST (useful for debugging)\n\
        \  -h=file.h    header files to be included in translation to C++\n\
        \                 this header files is intended use with external functions\n\
        \  -d           interactive debug mode\n\
        \  -e           enter debug mode on error\n\
        \                 (type \"h[elp]\" for options)"

parseArgs :: IO (Maybe ([String], Options))
parseArgs =
  do args <- getArgs
     (flags, files) <- return $ splitArgs args
     case checkFlags flags of
       Left err   -> putStrLn err >> return Nothing
       Right opts -> return $ Just (files, opts)

splitArgs :: [String] -> ([String], [String])
splitArgs = partition (\arg -> head arg == '-' && length arg > 1)

checkFlags :: [String] -> Either String Options
checkFlags = foldM addOption defaults

addOption :: Options -> String -> Either String Options
addOption opts@(Options { evalOpts = evalOptions }) "-m" =
  return $ opts { evalOpts = evalOptions { modInt = (ModPow2 32) } }
addOption opts@(Options { evalOpts = evalOptions }) ('-':'m':n) =
  case reads n of
    [(nVal, "")] -> return $ opts { evalOpts = evalOptions { modInt = (ModPow2 nVal) } }
    _               -> Left "Non-number given to -m option"
addOption opts@(Options { evalOpts = evalOptions }) "-p" =
  return $ opts { evalOpts = evalOptions { modInt = (ModPrime 2147483647) } }
addOption opts@(Options { evalOpts = evalOptions }) ('-':'p':n) =
  case reads n of
    [(nVal, "")] ->
      case isPrime $ toInteger nVal of
        True -> return $ opts { evalOpts = evalOptions { modInt = (ModPrime nVal) } }
        False -> Left "Non-prime given to -p option"
    _            -> Left "Non-number given to -p option"
addOption opts ('-':'t':time) =
  case reads time of
    [(timeVal, "")] -> return $ opts { timeOut = timeVal }
    _               -> Left "Non-number given to -t option"
addOption opts "-a" = return opts { ast = True }
addOption opts "-i" = return opts { invert = True }
addOption opts "-c" = return opts { cCode = True }
addOption opts ('-':'h':'=':headerfile) = return opts { header = Just headerfile }
addOption opts@(Options { evalOpts = evalOptions }) "-d" =
 return $ opts { evalOpts = evalOptions {runDebugger = DebugOn } }
addOption opts@(Options { evalOpts = evalOptions }) "-e" =
 return $ opts { evalOpts = evalOptions {runDebugger = DebugError } }
addOption _ f = Left $ "invalid option: " ++ f

loadFile :: String -> IO String
loadFile "-"      = getContents
loadFile filename = readFile filename

printInverted :: String -> IO ()
printInverted filename =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ invertProgram prog

printAST :: String -> IO ()
printAST filename =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print prog

printCcode :: String -> Maybe String -> IO ()
printCcode filename headerfile =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ JTC.formatProgram headerfile prog

printInvertedCcode :: String -> Maybe String -> IO ()
printInvertedCcode filename headerfile =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ JTC.formatProgram headerfile $ invertProgram prog

parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = do args <- parseArgs
          case args of
            Just ([file], Options { cCode = True, invert = True, header = h }) -> printInvertedCcode file h
            Just ([file], Options { cCode = True, header = h }) -> printCcode file h
            Just ([file], Options { ast = True }) -> printAST file
            Just ([file], Options { invert = True }) -> printInverted file
            Just ([file], opts) ->
              do res <- timeout (timeOut opts * 1000000)
                                (parseAndRun file (evalOpts opts))
                 case res of
                   Nothing -> exitWith $ ExitFailure 124
                   _       -> return ()
            _ -> putStrLn usage
