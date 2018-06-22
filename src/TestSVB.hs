{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment
import System.Exit
import System.IO
--import System.Timeout
import Control.Monad
import Data.List
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..))
import Jana.Invert

import Data.SBV
import Language.Haskell.TH


data Options = Options
  { timeOutt :: Int
  , invert :: Bool
  , evalOpts :: EvalOptions }

defaults = Options
  { timeOutt = -1
  , invert = False
  , evalOpts = defaultOptions }

usage = "usage: jana [options] <file>\n\
        \options:\n\
        \  -m           use 32-bit modular arithmetic\n\
        \  -tN          timeout after N seconds\n\
        \  -i           print inverted program"

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
addOption opts ('-':'t':time) =
  case reads time of
    [(timeVal, "")] -> return $ opts { timeOutt = timeVal }
    _               -> Left "non-number given to -t option"
addOption opts "-i" = return opts { invert = True }
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

parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = 
  do --res <- parseAndRun file (evalOpts opts)
--     p <- prove $ \(y::SWord8) -> y `shiftL` 2 .== 5*y
     --runQ (cnst 1 "x")
     p <- prove $ forAll ["try"] $ runQ [| try |]
     putStrLn $ show p
     return ()
  where
    f :: SWord8 -> SWord8 -> SBV Bool
    f x y = (x + y) `shiftL` 2 .== 4*(x + y)

    try :: SWord8 -> SBV Bool
    try x = ite ((add x 2) .== 4) 
                (x - 1 .== 3)
                (x ./= 3)

    add :: SWord8 -> SWord8 -> SWord8
    add x y = x + y

    -- ite :: SBV Bool -> SBV Bool -> SBV Bool -> SBV Bool
    -- ite c a b = (c ==> a) &&& ( (bnot c) ==> (bnot b))


    -- try += 2
    -- if try = 4 then
    --     try -= 1
    -- fi try = 3