{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Types (
    Array, Stack, Index, StoreEntry,
    Value(..), IntValue(..), nil, performOperation, performModOperation,
    unpackIntValue, intTypeToValueType, valueToValueType, typeToValueType, valueToIntType, zeroValue,
    showValueType, typesMatch, truthy, findIndex,
    Store, printVdecl, showCurrentStore, showStore, emptyStore, storeFromList, putStore, getStore,
    getRefVal, getEntry, getEntryIndex, getVar, getRefValue, bindVar, unbindVar, setVar,
    EvalEnv(..),
    EvalOptions(..), defaultOptions, DebugMode(..),
    ProcEnv, emptyProcEnv, procEnvFromList, getProc,
    Eval, runEval, (<!!>),
    BreakPoints, checkLine, EvalState, ModEval(..),
    checkForBreak, addBreakPoint, removeBreakPoint, isDebuggerRunning, whenDebugging,
    whenDebuggingElse, doWhenDebugging, whenFirstBreak, whenFullDebugging, isErrorDebugging,
    executeForward, executeBackward, flipExecution, whenForwardExecution, whenBackwardExecution,
    isForwardExecution, whenForwardExecutionElse, isUserForwardExecution, doWhenForwardExecution,
    stepForward, stepBackward,
    getFreshVar
    ) where

import Control.Applicative
import Prelude hiding (GT, LT, EQ)
import Data.Bits
import Data.List (intercalate, genericSplitAt)
import Data.IORef
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Text.Printf (printf)
import qualified Data.Map as Map
import Math.NumberTheory.GCD (extendedGCD)
import Data.Int
import Data.Word

import Text.Parsec.Pos

import Jana.Aliases
import Jana.Ast
import Jana.Error
import Jana.ErrorMessages

-- import Debug.Trace (trace)

type Array = [IntValue]
type Stack = [IntValue]
type Index = [Integer]

-- Types of values an expression can evaluate to.
data Value
  = JInt IntValue
  | JBool Bool
  | JArray Index Array
  | JStack Stack
  deriving (Eq)

data IntValue
  = JUnbound  Integer
  | JI8       Int8
  | JI16      Int16
  | JI32      Int32
  | JI64      Int64
  | JU8       Word8
  | JU16      Word16
  | JU32      Word32
  | JU64      Word64
  | JInferInt Integer
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

instance Show IntValue where
  show (JUnbound  i) = show i
  show (JI8       i) = show i
  show (JI16      i) = show i
  show (JI32      i) = show i
  show (JI64      i) = show i
  show (JU8       i) = show i
  show (JU16      i) = show i
  show (JU32      i) = show i
  show (JU64      i) = show i
  show (JInferInt i) = show i

zeroValue :: IntType -> IntValue
zeroValue t = f t 0
  where
    f :: IntType -> Integer -> IntValue
    f = intTypeToValueType

unpackIntValue :: IntValue -> Integer
unpackIntValue (JUnbound  i) = i
unpackIntValue (JI8       i) = toInteger i
unpackIntValue (JI16      i) = toInteger i
unpackIntValue (JI32      i) = toInteger i
unpackIntValue (JI64      i) = toInteger i
unpackIntValue (JU8       i) = toInteger i
unpackIntValue (JU16      i) = toInteger i
unpackIntValue (JU32      i) = toInteger i
unpackIntValue (JU64      i) = toInteger i
unpackIntValue (JInferInt i) = toInteger i

typeToValueType :: Integral a => Type -> a -> IntValue
typeToValueType (Int itype _) = intTypeToValueType itype
typeToValueType (Stack _)     = JUnbound . fromIntegral
typeToValueType (BoolT _)     = JUnbound . fromIntegral

intTypeToValueType :: Integral a => IntType -> a -> IntValue
intTypeToValueType FreshVar = error "Fresh variables must be inferred before this."
intTypeToValueType Unbound  = JUnbound . fromIntegral
intTypeToValueType I8       = JI8 . fromIntegral
intTypeToValueType I16      = JI16 . fromIntegral
intTypeToValueType I32      = JI32 . fromIntegral
intTypeToValueType I64      = JI64 . fromIntegral
intTypeToValueType U8       = JU8 . fromIntegral
intTypeToValueType U16      = JU16 . fromIntegral
intTypeToValueType U32      = JU32 . fromIntegral
intTypeToValueType U64      = JU64 . fromIntegral
intTypeToValueType InferInt = JInferInt . fromIntegral

valueToValueType :: Integral a => Value -> a -> IntValue
valueToValueType (JInt ival)   = intTypeToValueType (intValueToIntType ival)
valueToValueType (JBool _)     = JUnbound . fromIntegral
valueToValueType (JArray _ a)  = intTypeToValueType (intValueToIntType $ head a)
valueToValueType (JStack s)    = intTypeToValueType (intValueToIntType $ head s)

valueToIntType :: Value -> IntType
valueToIntType (JInt ival)   = intValueToIntType ival
valueToIntType (JBool _)     = Unbound
valueToIntType (JArray _ a)  = intValueToIntType $ head a
valueToIntType (JStack s)    = intValueToIntType $ head s

intValueToIntType :: IntValue -> IntType
intValueToIntType (JUnbound  _) = Unbound
intValueToIntType (JI8       _) = I8
intValueToIntType (JI16      _) = I16
intValueToIntType (JI32      _) = I32
intValueToIntType (JI64      _) = I64
intValueToIntType (JU8       _) = U8
intValueToIntType (JU16      _) = U16
intValueToIntType (JU32      _) = U32
intValueToIntType (JU64      _) = U64
intValueToIntType (JInferInt _) = InferInt

intValueToValueType :: Integral a => IntValue -> a -> IntValue
intValueToValueType v = intTypeToValueType $ intValueToIntType v

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
showValueType (JInt i) = showIntValue i
showValueType (JStack _)   = "stack"
showValueType (JArray i _) = "array" ++ "[" ++ intercalate ", " (map show i) ++ "]"
showValueType (JBool _)    = "bool"

showIntValue :: IntValue -> [Char]
showIntValue (JUnbound  _) = "int"
showIntValue (JI8       _) = "i8"
showIntValue (JI16      _) = "i16"
showIntValue (JI32      _) = "i32"
showIntValue (JI64      _) = "i64"
showIntValue (JU8       _) = "u8"
showIntValue (JU16      _) = "u16"
showIntValue (JU32      _) = "u32"
showIntValue (JU64      _) = "u64"
showIntValue (JInferInt _) = error "Inferred ints must be inferred before."

typesMatch :: Value -> Value -> Bool
typesMatch (JInt i1) (JInt i2) = typesMatchInt i1 i2
typesMatch (JArray i1 _) (JArray i2 _)
  | i1 == i2  = True
  | otherwise = False
typesMatch (JStack _) (JStack _) = True
typesMatch (JBool _)  (JBool _)  = True
typesMatch _          _          = False

typesMatchInt :: IntValue -> IntValue -> Bool
typesMatchInt (JUnbound  _) (JUnbound _)  = True
typesMatchInt (JI8       _) (JI8      _)  = True
typesMatchInt (JI16      _) (JI16     _)  = True
typesMatchInt (JI32      _) (JI32     _)  = True
typesMatchInt (JI64      _) (JI64     _)  = True
typesMatchInt (JU8       _) (JU8      _)  = True
typesMatchInt (JU16      _) (JU16     _)  = True
typesMatchInt (JU32      _) (JU32     _)  = True
typesMatchInt (JU64      _) (JU64     _)  = True
typesMatchInt (JInferInt _) _             = True
typesMatchInt _             (JInferInt _) = True
typesMatchInt _              _            = False

nil :: Value
nil = JStack []

truthy :: Value -> Bool
truthy (JInt i)    = truthyInt i
truthy (JStack []) = False
truthy _           = True

truthyInt :: IntValue -> Bool
truthyInt (JUnbound 0) = True
truthyInt (JI8      0) = True
truthyInt (JI16     0) = True
truthyInt (JI32     0) = True
truthyInt (JI64     0) = True
truthyInt (JU8      0) = True
truthyInt (JU16     0) = True
truthyInt (JU32     0) = True
truthyInt (JU64     0) = True
truthyInt _            = False

wrap :: (a -> Value) -> (i -> i -> a) -> i -> i -> Value
wrap m f x y = m $ f x y

opFunc :: (Bits b, Integral b) => BinOp -> (b -> IntValue) -> b -> b -> Value
opFunc Add f = wrap (JInt . f) (+)
opFunc Sub f = wrap (JInt . f) (-)
opFunc Mul f = wrap (JInt . f) (*)
opFunc Exp f = wrap (JInt . f) (^)
opFunc Div f = wrap (JInt . f) div
opFunc Mod f = wrap (JInt . f) mod
opFunc And f = wrap (JInt . f) (.&.)
opFunc Or  f = wrap (JInt . f) (.|.)
opFunc Xor f = wrap (JInt . f) xor
opFunc SL  f = wrap (JInt . f) (\x y -> shiftL x (fromEnum y))
opFunc SR  f = wrap (JInt . f) (\x y -> shiftR x (fromEnum y))
opFunc GT  _ = wrap JBool (>)
opFunc LT  _ = wrap JBool (<)
opFunc EQ  _ = wrap JBool (==)
opFunc NEQ _ = wrap JBool (/=)
opFunc GE  _ = wrap JBool (>=)
opFunc LE  _ = wrap JBool (<=)
opFunc _   _ = undefined -- handled by evalExpr

performOperation :: BinOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
--performOperation modOp v1 v2 _ _ | trace ("binOp " ++ show v1 ++ " " ++ show modOp ++ " " ++ show v2) False = undefined
performOperation Div (JInt _) (JInt ival) _ pos | truthyInt ival =
  pos <!!> divisionByZero
performOperation Div (JInt (JUnbound i1)) (JInt (JUnbound i2)) _ _ =
  do flag <- asks (modInt . evalOptions)
     case flag of
       (ModPrime n) -> return $ opFunc Mul (JUnbound) i1 (multInv i2 n)
       _ -> return $ opFunc Div (JUnbound) i1 i2
  where
    multInv a p = (\(_,i,_) -> i) $ extendedGCD a (toInteger p)
performOperation SL (JInt ival)           (JInt shiftval)       _  _  =
  return $ opFunc SL (intValueToValueType ival) (unpackIntValue ival) (unpackIntValue shiftval)
performOperation SR (JInt ival)           (JInt shiftval)       _  _  =
  return $ opFunc SR (intValueToValueType ival) (unpackIntValue ival) (unpackIntValue shiftval)
performOperation op (JInt (JUnbound i1))  (JInt (JUnbound i2))  _  _  = return $ opFunc op (JUnbound)  i1 i2
performOperation op (JInt (JI8 i1))       (JInt (JI8 i2))       _  _  = return $ opFunc op (JI8)       i1 i2
performOperation op (JInt (JI16 i1))      (JInt (JI16 i2))      _  _  = return $ opFunc op (JI16)      i1 i2
performOperation op (JInt (JI32 i1))      (JInt (JI32 i2))      _  _  = return $ opFunc op (JI32)      i1 i2
performOperation op (JInt (JI64 i1))      (JInt (JI64 i2))      _  _  = return $ opFunc op (JI64)      i1 i2
performOperation op (JInt (JU8 i1))       (JInt (JU8 i2))       _  _  = return $ opFunc op (JU8)       i1 i2
performOperation op (JInt (JU16 i1))      (JInt (JU16 i2))      _  _  = return $ opFunc op (JU16)      i1 i2
performOperation op (JInt (JU32 i1))      (JInt (JU32 i2))      _  _  = return $ opFunc op (JU32)      i1 i2
performOperation op (JInt (JU64 i1))      (JInt (JU64 i2))      _  _  = return $ opFunc op (JU64)      i1 i2
performOperation op (JInt (JInferInt i1)) (JInt (JInferInt i2)) _  _  = return $ opFunc op (JInferInt) i1 i2
performOperation op (JInt (JInferInt i1)) (JInt ival)           p1 p2 =
  performOperation op (JInt (intTypeToValueType (intValueToIntType ival) i1)) (JInt ival) p1 p2
performOperation op (JInt ival)           (JInt (JInferInt i2)) p1 p2 =
  performOperation op (JInt ival) (JInt (intTypeToValueType (intValueToIntType ival) i2)) p1 p2
performOperation _ val1 val2 _ pos =
  pos <!!> typeMismatch [showValueType val1] (showValueType val2)

performModOperation :: ModOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
-- performModOperation modOp v1 v2 _ _ | trace ("modOp " ++ show v1 ++ " " ++ show modOp ++ " " ++ show v2) False = undefined
performModOperation modOp a b c d = performOperation (modOpToBinOp modOp) a b c d
  where modOpToBinOp AddEq = Add
        modOpToBinOp SubEq = Sub
        modOpToBinOp XorEq = Xor

--------------------------------------------------------------
-- Environment
--------------------------------------------------------------

data EvalState = ES { breakPoints :: BreakPoints
                    , forwardExecution :: Bool
                    , userForwardExecution :: Bool
                    , firstDBbeginning :: Bool
                    , stepDebugging :: Bool
                    , freshVarIdx :: Int
                    , store :: Store}

emptyStore :: EvalState
emptyStore = ES { breakPoints = Set.empty,
                  userForwardExecution = True,
                  forwardExecution = True,
                  firstDBbeginning = True,
                  stepDebugging = False,
                  freshVarIdx = 0,
                  store = Map.empty}

getFreshVar :: SourcePos -> Eval Ident
getFreshVar p =
  do env <- get
     let i = freshVarIdx env
     modify $ \x -> x {freshVarIdx = i + 1}
     return $ Ident ("_eval_tmp_" ++ show i) p

--------------------------------------------------------------
-- Debugger commands
--------------------------------------------------------------

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
  do evalS <- get
     let step = stepDebugging evalS
     when step $ modify $ \x -> x {stepDebugging = False}
     return $ or [step, Set.member (sourceLine s) (breakPoints evalS)]

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

stepForward :: Eval ()
stepForward =
  modify $ \x -> x {stepDebugging = True, userForwardExecution = True, forwardExecution = userForwardExecution x}

stepBackward :: Eval ()
stepBackward =
  modify $ \x -> x {stepDebugging = True, userForwardExecution = False, forwardExecution = not (userForwardExecution x)}

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

--------------------------------------------------------------
-- Store
--------------------------------------------------------------

data EntryType = Static | Dynamic
  deriving (Eq)

type StoreEntry = (IORef Value, Index, EntryType)
type Store = Map.Map String StoreEntry

getStore :: Eval Store
getStore =
  do evalS <- get
     return $ store evalS

putStore :: Store -> Eval()
putStore s =
  do evalS <- get
     put $ evalS {store = s}

printVdecl :: String -> Value -> String
printVdecl name val@(JArray i _) = printf "%s%s = %s" name (concatMap (\x -> "["++ show x ++ "]") i) (show val)
printVdecl name val = printf "%s = %s" name (show val)

showStore :: EvalState -> IO String
showStore s =
  liftM (intercalate "\n")
        (mapM (\(name, (ref,_,_)) -> liftM (printVdecl name) (readIORef ref))
              (Map.toList (store s)))

showCurrentStore :: Eval String
showCurrentStore =
  do evalS <- get
     let storeEnv = store evalS
         sList = Map.toList storeEnv
     ents <- mapM mapFun sList
     return $ intercalate "\n" ents
  where
    mapFun (s,(r,_,_)) =
      do v <- getRefValue r
         return $ printVdecl s v

storeFromList :: [(String, (IORef Value, Index, EntryType))] -> Store
storeFromList = Map.fromList

getEntry :: Ident -> Eval StoreEntry
getEntry (Ident name pos) =
  do evalS <- get
     let storeEnv = store evalS
     case Map.lookup name storeEnv of
       Just ref -> return ref
       Nothing  -> pos <!!> unboundVar name

getEntryIndex :: Ident -> Eval Index
getEntryIndex idnt =
  do (_,i,_) <- getEntry idnt
     return i

-- getRef :: Ident -> Eval (IORef Value)
-- getRef idnt = getRef idnt >>= liftIO . readIORef

getRefVal :: Ident -> Eval Value
getRefVal idnt =
  do (r,_,_) <- getEntry idnt
     getRefValue r

getVar :: Ident -> Eval Value
getVar idnt =
  do (r,i,_) <- getEntry idnt
     v <- getRefValue r
     return $ indexValue i v

getRefValue :: IORef Value -> Eval Value
getRefValue = liftIO . readIORef

indexValue :: Index -> Value -> Value
indexValue [] v = v
indexValue [i] (JArray [idx] array) | i < idx = JInt $ head $ drop (fromInteger i) array
indexValue (i:is) (JArray (idx:idxs) array) | i < idx =
  indexValue is (JArray idxs (take (idxSize) $ drop ((fromInteger i)*idxSize) array))
  where
    idxSize = div (length array) (fromInteger idx)
indexValue _ _ = undefined "Indexing non-array value"

-- Bind a variable name to a new reference
bindVar :: VdeclType -> Ident -> Value -> Eval ()
bindVar vdType (Ident name pos) val =
  do evalS <- get
     let storeEnv = store evalS
     ref <- liftIO $ newIORef val
     case Map.lookup name storeEnv of
       Nothing  -> put $ evalS {store = Map.insert name (ref, [], entType vdType) storeEnv}
       Just _   -> pos <!!> alreadyBound name
  where 
    entType Constant = Static
    entType _        = Dynamic

unbindVar :: Ident -> Eval ()
unbindVar i =
  do evalS <- get
     let storeEnv = store evalS
     put $ evalS {store = Map.delete (ident i) storeEnv}

-- Set the value of a variable (modifying the reference)
setVar :: Ident -> Value -> Eval ()
setVar idnt val =
  do (ref, i, etype) <- getEntry idnt
     when (etype == Static) $ error "Updating constant"
     curV <- getRefValue ref
     liftIO $ writeIORef ref $ updValue i curV val

updValue :: Index -> Value -> Value -> Value
-- updValue i v1 v2 | trace ("updValue index: " ++ show i ++ " " ++ show v1 ++ " " ++ show v2) False = undefined
updValue _ _ v = v
-- This was not needed anyway. 
-- updValue is (JInt val) (JArray idxs array) = 
--   case findIndex is idxs of
--     Nothing  -> error ""
--     (Just i) -> JArray idxs (take (fromInteger i) array ++ [val] ++ (drop ((fromInteger i)+1) array))

-- Reader

data EvalEnv = EE { evalOptions :: EvalOptions
                  , procEnv :: ProcEnv
                  , aliases :: AliasSet}

data ModEval = None | ModPow2 Int | ModPrime Int
  deriving (Eq)

data EvalOptions = EvalOptions { modInt :: ModEval, runReverse :: Bool, runDebugger :: DebugMode}

defaultOptions :: EvalOptions
defaultOptions   = EvalOptions { modInt = None, runReverse = False, runDebugger = DebugOff }

data DebugMode = DebugOff | DebugOn | DebugError
  deriving (Eq)

type ProcEnv = Map.Map String Proc

emptyProcEnv :: Map.Map k a
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
makeIdentList (Proc {params = p}) = map getVdeclIdent p
  where
    getVdeclIdent (Scalar _ _ idnt _ _)   = idnt
    getVdeclIdent (Array  _ _ idnt _ _ _) = idnt

checkDuplicateArgs :: [Ident] -> Bool
checkDuplicateArgs []         = True
checkDuplicateArgs ([_])    = True
checkDuplicateArgs (arg:args) =
  arg `notElem` args && checkDuplicateArgs args

getProc :: Ident -> Eval Proc
getProc (Ident funName pos) =
  do when (funName == "main") $ pos <!!> callingMainError
     procE <- asks procEnv
     case Map.lookup funName procE of
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
runEval eval eState procs = runExceptT (runReaderT (runStateT (runE eval) eState) procs)

throwJanaError :: SourcePos -> Message -> Eval a
throwJanaError pos msg = throwError $ newErrorMessage pos msg

(<!!>) :: SourcePos -> Message -> Eval a
infixr 1 <!!>
pos <!!> msg = throwJanaError pos msg
