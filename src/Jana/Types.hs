{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Types (
    Array, Stack, Index,
    Value(..), nil, performOperation, performModOperation,
    showValueType, typesMatch, truthy, findIndex,
    Store, printVdecl, showStore, emptyStore, storeFromList, putStore, getStore,
    getRef, getVar, getRefValue, bindVar, unbindVar, setVar,
    EvalEnv(..),
    EvalOptions(..), defaultOptions, DebugMode(..),
    ProcEnv, emptyProcEnv, procEnvFromList, getProc,
    Eval, runEval, (<!!>),
    BreakPoints, checkLine, EvalState, ModEval(..),
    checkForBreak, addBreakPoint, removeBreakPoint, isDebuggerRunning, whenDebugging, 
    whenDebuggingElse, doWhenDebugging, whenFirstBreak, whenFullDebugging, isErrorDebugging,
    executeForward, executeBackward, flipExecution, whenForwardExecution, whenBackwardExecution, 
    isForwardExecution, whenForwardExecutionElse, isUserForwardExecution,
    ) where

import Prelude hiding (GT, LT, EQ)
import Data.Bits
import Data.List (intercalate, genericSplitAt)
import Data.IORef
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Text.Printf (printf)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Math.NumberTheory.GCD (extendedGCD)

import Text.Parsec.Pos

import Jana.Aliases
import Jana.Ast
import Jana.Error
import Jana.ErrorMessages

type Array = [Integer]
type Stack = [Integer]
type Index = [Integer]

-- Types of values an expression can evaluate to.
data Value
  = JInt Integer
  | JBool Bool
  | JArray Index Array
  | JStack Stack
  deriving (Eq)

instance Show Value where
  show (JBool True)       = "true"
  show (JBool False)      = "false"
  show (JInt x)           = show x
  show (JArray [] _)      = ""
  show (JArray [_] xs)    = "{" ++ intercalate ", " (map show xs) ++ "}"
  show (JArray (i:is) xs) = "{" ++ intercalate ", " (map (\x -> show $ JArray is x) $ partitionInto i xs) ++ "}"
  show (JStack [])        = "nil"
  show (JStack xs)        = "<" ++ intercalate ", " (map show xs) ++ "]"

partitionInto :: Integer -> [a] -> [[a]]
partitionInto s n = go n
  where
    k = (toInteger $ length n) `div` s
    go t = case genericSplitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b

findIndex :: Index -> Index -> Maybe Integer
findIndex []       []       = Just 0
findIndex (iA:iAs) (iC:iCs) 
  | and [iA > iC, iC >= 0]  = (\x -> return $ product (iC:iAs) + x) =<< findIndex iAs iCs
  | otherwise               = Nothing
findIndex _        _        = Nothing

showValueType :: Value -> String
showValueType (JInt _)   = "int"
showValueType (JStack _) = "stack"
showValueType (JArray i _) = "array" ++ "[" ++ intercalate ", " (map show i) ++ "]"
showValueType (JBool _)  = "bool"

typesMatch :: Value -> Value -> Bool
typesMatch (JInt _)   (JInt _)   = True
typesMatch (JArray i1 _) (JArray i2 _) 
  | i1 == i2  = True
  | otherwise = False
typesMatch (JStack _) (JStack _) = True
typesMatch (JBool _)  (JBool _)  = True
typesMatch _          _          = False

nil = JStack []

truthy :: Value -> Bool
truthy (JInt 0)    = False
truthy (JStack []) = False
truthy _           = True


boolToInt :: Num a => (a -> a -> Bool) -> a -> a -> a
boolToInt f x y = if f x y then 1 else 0

wrap :: (a -> Value) -> (Integer -> Integer -> a) -> Integer -> Integer -> Value
wrap m f x y = m $ f x y


opFunc :: BinOp -> Integer -> Integer -> Value
opFunc Add  = wrap JInt (+)
opFunc Sub  = wrap JInt (-)
opFunc Mul  = wrap JInt (*)
opFunc Exp  = wrap JInt (^)
opFunc Div  = wrap JInt div
opFunc Mod  = wrap JInt mod
opFunc And  = wrap JInt (.&.)
opFunc Or   = wrap JInt (.|.)
opFunc Xor  = wrap JInt xor
opFunc LAnd = undefined -- handled by evalExpr
opFunc LOr  = undefined -- handled by evalExpr
opFunc GT   = wrap JBool (>)
opFunc LT   = wrap JBool (<)
opFunc EQ   = wrap JBool (==)
opFunc NEQ  = wrap JBool (/=)
opFunc GE   = wrap JBool (>=)
opFunc LE   = wrap JBool (<=)

performOperation :: BinOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
performOperation Div (JInt _) (JInt 0) _ pos =
  pos <!!> divisionByZero
performOperation Div (JInt x) (JInt y) _ pos =
  do flag <- asks (modInt . evalOptions)
     case flag of
       (ModPrime n) -> return $ opFunc Mul x (multInv y n)
       _ -> return $ opFunc Div x y
  where
    multInv a p = (\(_,i,_) -> i) $ extendedGCD a (toInteger p)
performOperation op (JInt x) (JInt y) _ _ =
  return $ opFunc op x y
performOperation _ (JInt _) val _ pos =
  pos <!!> typeMismatch ["int"] (showValueType val)
performOperation _ val _ pos _ =
  pos <!!> typeMismatch ["int"] (showValueType val)

performModOperation :: ModOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
performModOperation modOp = performOperation $ modOpToBinOp modOp
  where modOpToBinOp AddEq = Add
        modOpToBinOp SubEq = Sub
        modOpToBinOp XorEq = Xor

--
-- Environment
--

data EvalState = ES { breakPoints :: BreakPoints
                    , forwardExecution :: Bool
                    , userForwardExecution :: Bool
                    , firstDBbeginning :: Bool
                    , store :: Store}


emptyStore = ES { breakPoints = Set.empty,
                  userForwardExecution = True, 
                  forwardExecution = True, 
                  firstDBbeginning = True, 
                  store = Map.empty}

-- Break points

type BreakPoints = Set.Set Line

whenFirstBreak :: Eval () -> Eval () -> Eval ()
whenFirstBreak op1 op2 =
  do 
    env <- get
    if firstDBbeginning env
      then 
        do modify $ \x -> x {firstDBbeginning = False}
           op1
      else op2


checkLine :: Line -> SourcePos -> Bool
checkLine l p =
  l == sourceLine p

checkForBreak :: SourcePos -> Eval Bool
checkForBreak s =
  do evalState <- get
     return $ Set.member (sourceLine s) (breakPoints evalState)

removeBreakPoint :: Line -> Eval ()
removeBreakPoint l = 
  modify $ \x -> x {breakPoints = Set.delete l (breakPoints x)}

addBreakPoint :: Line -> Eval ()
addBreakPoint l = 
  modify $ \x -> x {breakPoints = Set.insert l (breakPoints x)}

executeForward :: Eval ()
executeForward =
  modify $ \x -> x {userForwardExecution = True, forwardExecution = userForwardExecution x}

executeBackward :: Eval ()
executeBackward =
  modify $ \x -> x {userForwardExecution = False, forwardExecution = not (userForwardExecution x)}

isForwardExecution :: Eval Bool
isForwardExecution = 
  do env <- get
     return $ forwardExecution env

isUserForwardExecution :: Eval Bool
isUserForwardExecution = 
  do env <- get
     return $ userForwardExecution env

flipExecution :: Eval ()
flipExecution =
  modify $ \x -> x {forwardExecution = not (forwardExecution x)}


doWhen :: Monad m => Bool -> a -> (a -> m a) -> m a
doWhen b a f =
  if b
    then return a
    else f a

doWhenForwardExecution :: a -> (a -> Eval a) -> Eval a
doWhenForwardExecution a f =
  do env <- get
     doWhen (forwardExecution env) a f

whenForwardExecution :: Eval () -> Eval ()
whenForwardExecution f =
  do env <- get
     when (forwardExecution env) f

whenForwardExecutionElse :: Eval () -> Eval () -> Eval ()
whenForwardExecutionElse f e =
  do env <- get
     if forwardExecution env
       then f
       else e
whenBackwardExecution :: Eval () -> Eval ()
whenBackwardExecution f =
  do env <- get
     unless (forwardExecution env) f


-- Store

type Store = Map.Map String (IORef Value)

getStore :: Eval Store
getStore =
  do evalState <- get
     return $ store evalState

putStore :: Store -> Eval()
putStore s =
  do evalState <- get
     put $ evalState {store = s}

printVdecl :: String -> Value -> String
printVdecl name val@(JArray i xs) = printf "%s%s = %s" name (concatMap (\x -> "["++ show x ++ "]") i) (show val)
printVdecl name val = printf "%s = %s" name (show val)

showStore :: EvalState -> IO String
showStore s =
  liftM (intercalate "\n")
        (mapM (\(name, ref) -> liftM (printVdecl name) (readIORef ref))
              (Map.toList (store s)))

storeFromList :: [(String, IORef Value)] -> Store
storeFromList = Map.fromList

getRef :: Ident -> Eval (IORef Value)
getRef (Ident name pos) =
  do evalState <- get
     let storeEnv = store evalState
     case Map.lookup name storeEnv of
       Just ref -> return ref
       Nothing  -> pos <!!> unboundVar name

getVar :: Ident -> Eval Value
getVar id = getRef id >>= liftIO . readIORef

getRefValue :: IORef Value -> Eval Value
getRefValue = liftIO . readIORef

-- Bind a variable name to a new reference
bindVar :: Ident -> Value -> Eval ()
bindVar (Ident name pos) val =
  do evalState <- get
     let storeEnv = store evalState
     ref <- liftIO $ newIORef val
     case Map.lookup name storeEnv of
       Nothing  -> put $ evalState {store = Map.insert name ref storeEnv}
       Just _   -> pos <!!> alreadyBound name

unbindVar :: Ident -> Eval ()
unbindVar i = 
  do evalState <- get
     let storeEnv = store evalState
     put $ evalState {store = Map.delete (ident i) storeEnv}

-- Set the value of a variable (modifying the reference)
setVar :: Ident -> Value -> Eval ()
setVar id val =
  do ref <- getRef id
     liftIO $ writeIORef ref val


-- Reader

data EvalEnv = EE { evalOptions :: EvalOptions
                  , procEnv :: ProcEnv
                  , aliases :: AliasSet}

data ModEval = None | ModPow2 Int | ModPrime Int
  deriving (Eq)

data EvalOptions = EvalOptions { modInt :: ModEval, runReverse :: Bool, runDebugger :: DebugMode}
defaultOptions   = EvalOptions { modInt = None, runReverse = False, runDebugger = DebugOff }

data DebugMode = DebugOff | DebugOn | DebugError
  deriving (Eq)

type ProcEnv = Map.Map String Proc

emptyProcEnv = Map.empty

procEnvFromList :: [Proc] -> Either JanaError ProcEnv
procEnvFromList = foldM insertProc emptyProcEnv
  where insertProc env p = if Map.notMember (ident p) env
                             then if checkDuplicateArgs (makeIdentList p)
                                    then Right (Map.insert (ident p) p env)
                                    else Left $ newErrorMessage (ppos p) (procDuplicateArgs p)
                             else Left  $ newErrorMessage (ppos p) (procDefined p)
        ppos  Proc { procname = (Ident _ pos) } = pos

makeIdentList :: Proc -> [Ident]
makeIdentList (Proc {params = params}) = map getVdeclIdent params
  where
    getVdeclIdent (Scalar _ id _ _) = id
    getVdeclIdent (Array id _ _ _)  = id

checkDuplicateArgs :: [Ident] -> Bool
checkDuplicateArgs []         = True
checkDuplicateArgs ([arg])    = True
checkDuplicateArgs (arg:args) =
  arg `notElem` args && checkDuplicateArgs args

getProc :: Ident -> Eval Proc
getProc (Ident funName pos) =
  do when (funName == "main") $ pos <!!> callingMainError
     procEnv <- asks procEnv
     case Map.lookup funName procEnv of
       Just proc -> return proc
       Nothing   -> pos <!!> undefProc funName

isDebuggerRunning :: Eval Bool
isDebuggerRunning =
  do env <- ask
     return $ db $ runDebugger $ evalOptions env
  where
    db DebugOff = False
    db _ = True

isErrorDebugging :: Eval Bool
isErrorDebugging =
  do env <- ask
     return $ db $ runDebugger $ evalOptions env
  where
    db DebugError = True
    db _ = False

whenFullDebugging :: Eval () -> Eval ()
whenFullDebugging op =
  do env <- ask
     when (db $ runDebugger $ evalOptions env) op
  where
    db DebugOn = True
    db _ = False

doWhenDebugging :: a -> (a -> Eval a) -> Eval a
doWhenDebugging a op =
  do db <- isDebuggerRunning
     doWhen db a op


whenDebugging :: Eval () -> Eval ()
whenDebugging op =
  do db <- isDebuggerRunning
     when db op

whenDebuggingElse :: Eval () -> Eval () -> Eval ()
whenDebuggingElse op1 op2 =
  do db <- isDebuggerRunning
     if db 
       then op1
       else op2

--
-- Evaluation
--

newtype Eval a = E { runE :: StateT EvalState (ReaderT EvalEnv (ExceptT JanaError IO)) a }
               deriving (Applicative, Functor, Monad, MonadIO, MonadError JanaError, MonadReader EvalEnv, MonadState EvalState)

runEval :: Eval a -> EvalState -> EvalEnv -> IO (Either JanaError (a, EvalState))
runEval eval evalState procs = runExceptT (runReaderT (runStateT (runE eval) evalState) procs)

throwJanaError :: SourcePos -> Message -> Eval a
throwJanaError pos msg = throwError $ newErrorMessage pos msg

infixr 1 <!!>
pos <!!> msg = throwJanaError pos msg
