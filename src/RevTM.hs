
import System.Environment
import System.Exit
import System.IO
import System.Timeout
import Control.Monad
import Data.List
-- import Jana.ParserBasic
import qualified Jana.Parser as JP
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..))
import Jana.Invert
import Jana.Error
import qualified RevTM.JanusToTMC as JTTMC
import qualified RevTM.FrontParser as FP

import Text.Parsec.Error

import RevTM.Types

data Options = Options
  { timeOut :: Int
  , invert :: Bool
  , cCode  :: Bool
  , tmCCode :: Bool
  , evalOpts :: EvalOptions }

defaults = Options
  { timeOut = -1
  , invert = False
  , cCode = False
  , tmCCode = True
  , evalOpts = defaultOptions }

usage = "usage: revTM <file>"
        -- \options:\n\
        -- \  -m           use 32-bit modular arithmetic\n\
        -- \  -tN          timeout after N seconds\n\
        -- \  -i           print inverted program\n\
        -- \  -c           print C program\n\
        -- \  -tm          print C program"

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
  return $ opts { evalOpts = evalOptions { modInt = True } }
addOption opts "-i"  = return opts { invert = True }
addOption opts "-c"  = return opts { cCode = True }
addOption opts "-tm" = return opts { tmCCode = True }
addOption _ f = Left $ "invalid option: " ++ f

loadFile :: String -> IO String
loadFile "-"      = getContents
loadFile filename = readFile filename

printRevTMCode :: String -> IO ()
printRevTMCode filename =
  do text <- loadFile filename
     case FP.parseString filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> 
         case sequence $ map (formatRevTMPart filename) prog of
           Left err -> print err >> (exitWith $ ExitFailure 1)
           Right prog -> putStrLn $ concat prog
       -- Right prog -> show $ JTTMC.formatProgram $ invertProgram prog

formatRevTMPart :: String -> RevTMBlock -> Either JanaError String
formatRevTMPart _        (Csect str) = Right str
formatRevTMPart filename (RevTMsect inputs vars prog pos) =
  case JP.parseStmts pos filename prog of
    Left   err   -> Left err
    Right  jprog -> Right $ show $ JTTMC.formatRevTM vars jprog

main :: IO ()
main = do args <- parseArgs
          case args of
            Just ([file], Options { tmCCode = True }) -> printRevTMCode file
            -- Just ([file], Options { invert = True }) -> printInverted file
            _ -> putStrLn usage
