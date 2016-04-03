module Jana.Eval (
  runProgram,
  evalLval,
  evalExpr,
  runEval,
  ) where


import Prelude hiding (GT, LT, EQ, userError)
import System.Exit
import Data.Char (toLower, isSpace)
import Data.List (genericSplitAt, genericReplicate, intercalate, genericTake,  genericIndex)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield, yield)

import Text.Parsec.Pos

import Jana.Aliases
import Jana.Ast
import Jana.Types
import Jana.Invert
import Jana.Error
import Jana.ErrorMessages
import Jana.Parser (parseExprString, parseStmtsString)
import Jana.Printf
import Jana.Debug

inArgument :: String -> String -> Eval a -> Eval a
inArgument funid argid monad = catchError monad $
  throwError . addErrorMessage (InArgument funid argid)

inExpression :: Expr -> Eval a -> Eval a
inExpression expr monad = catchError monad $
  throwError . addOnceErrorMessage (InExpression expr)

inStatement :: Stmt -> Eval a -> Eval a
inStatement stmt monad = catchError monad $
  \err -> do storeStr <- get >>= liftIO . showStore
             throwError $ addErrorMessage (InStatement stmt storeStr) err

inProcedure :: Proc -> Eval a -> Eval a
inProcedure proc monad = catchError monad $
  throwError . addErrorMessage (InProcedure $ ident proc)


unpackInt :: SourcePos -> Value -> Eval Integer
unpackInt _ (JInt x) = return x
unpackInt pos val = pos <!!> typeMismatch ["int"] (showValueType val)

unpackArray :: SourcePos -> Value -> Eval (Index, Array)
unpackArray _ (JArray i x) = return (i,x)
unpackArray pos val = pos <!!> typeMismatch ["array"] (showValueType val)

unpackStack :: SourcePos -> Value -> Eval Stack
unpackStack _ (JStack x) = return x
unpackStack pos val = pos <!!> typeMismatch ["stack"] (showValueType val)

unpackBool :: SourcePos -> Value -> Eval Bool
unpackBool _ (JBool x) = return x
unpackBool pos val = pos <!!> typeMismatch ["bool"] (showValueType val)

assert :: Bool -> Expr -> Eval ()
assert bool expr =
  do val1 <- unpackBool (getExprPos expr) =<< evalModularExpr expr
     unless (val1 == bool) $
       getExprPos expr <!!> assertionFail ("should be " ++ map toLower (show bool))

assertTrue = assert True
assertFalse = assert False

checkType :: Type -> Value -> Eval ()
checkType (Int pos)   (JInt _)   = return ()
checkType (Int pos)   (JArray _ _) = return ()
checkType (Stack pos) (JStack _) = return ()
checkType (Int pos)   val = pos <!!> typeMismatch ["int"] (showValueType val)
checkType (Stack pos) val = pos <!!> typeMismatch ["stack"] (showValueType val)

checkTypeInt :: SourcePos -> Value -> Eval Integer
checkTypeInt pos (JInt v) = return v
checkTypeInt pos val      = pos <!!> typeMismatch ["int"] (showValueType val)

-- This is likely to be wrong
checkVdecl :: Vdecl -> Value -> Eval ()
checkVdecl (Scalar Int {}   _ _ _)  (JInt _)     = return ()
checkVdecl (Scalar Stack {} _ _ _)  (JStack _)   = return ()
checkVdecl (Array _ []  _ _)   (JArray _ _) = return ()
checkVdecl (Array _ sizeExpr _ pos) (JArray size arr) =
  do sizeInt <- evalSize pos sizeExpr (Just size)
     return ()
     -- val <- evalModularExpr x
     -- valInt <- checkTypeInt pos val
     -- unless (valInt == arrLen) $ pos <!!> arraySizeMismatch valInt arrLen
  -- where arrLen = toInteger (length arr)
checkVdecl vdecl val =
  vdeclPos vdecl <!!> typeMismatch [vdeclType vdecl] (showValueType val)
  where vdeclPos (Scalar _ _ _ pos) = pos
        vdeclPos (Array _ _ _ pos)  = pos
        vdeclType (Scalar Int{} _ _ _)   = "int"
        vdeclType (Scalar Stack{} _ _ _) = "stack"
        vdeclType (Array{})              = "array"


arrayLookup :: (Index, Array) -> [Integer] -> SourcePos -> Eval Value
arrayLookup (sIdx,arr) idx pos =
  case findIndex sIdx idx of
    Nothing   -> pos <!!> outOfBounds idx sIdx
    Just idx' -> return $ JInt $ genericIndex arr idx'

arrayModify :: SourcePos -> Index -> Array -> Index -> Integer -> Eval Array
arrayModify pos sIdx arr idx val = 
  case findIndex sIdx idx of
    Nothing   -> pos <!!> outOfBounds idx sIdx
    Just idx' -> return $ (\(xs, _:ys) -> xs ++ val : ys) $ genericSplitAt idx' arr
  

getExprPos :: Expr -> SourcePos
getExprPos (Number _ pos)  = pos
getExprPos (Boolean _ pos) = pos
getExprPos (LV _ pos)      = pos
getExprPos (UnaryOp _ e)   = getExprPos e
getExprPos (BinOp _ e1 _)  = getExprPos e1
getExprPos (Empty _ pos)   = pos
getExprPos (Top _ pos)     = pos
getExprPos (Size _ pos)    = pos
getExprPos (Nil pos)       = pos



runProgram :: String -> Program -> EvalOptions -> IO ()
runProgram _ p@(Program [main] procs) evalOptions =
  runProgramAfterDBcheck checkDB evalOptions
  where
    checkDB =
      if runDebugger evalOptions
        then injectDBProgram p
        else p 
runProgram filename (Program [] _) _ =
  print (newFileError filename noMainProc) >> exitWith (ExitFailure 1)
runProgram filename (Program _ _) _ =
  print (newFileError filename multipleMainProcs) >> exitWith (ExitFailure 1)

runProgramAfterDBcheck :: Program -> EvalOptions -> IO ()
runProgramAfterDBcheck (Program [main] procs) evalOptions =
  case procEnvFromList procs of
    Left err -> print err
    Right procEnv ->
      let env = EE { procEnv = procEnv
                   , evalOptions = evalOptions
                   , aliases = Jana.Aliases.empty}
      in do runRes <- runEval (evalMain main) emptyStore env
            case runRes of
              Right (_, s) -> showStore s >>= putStrLn
              Left err     -> print err >> exitWith (ExitFailure 1)

---- Array functions
-- Clean a bit up here

sizeEstimate :: Expr -> Index
sizeEstimate (ArrayE a _) = (toInteger $ length a):(sizeEstimate $ head a)
sizeEstimate _            = [] 

flattenArray :: SourcePos -> Index -> Expr -> Eval [Expr]
flattenArray _ [] (ArrayE _ pos) = pos <!!> arraySize
flattenArray _ [] e              = return [e]
flattenArray _ (s:sIdx) (ArrayE a pos)
  | s >= size = 
      do es <- liftM concat $ mapM (flattenArray pos sIdx) a
         return $ genericTake (product (s:sIdx)) $ es ++ (repeat $ Number 0 pos)
  | otherwise                    = pos <!!> arraySize
  where size = toInteger (length a)
flattenArray pos _ _             = pos <!!> arraySize

evalSize :: SourcePos -> [Maybe Expr] -> Maybe Index -> Eval Index
evalSize pos exprs index = evalAliasSize pos Nothing exprs index

evalAliasSize :: SourcePos -> Maybe Lval -> [Maybe Expr] -> Maybe Index -> Eval Index
evalAliasSize _ _ [] _ = return []
evalAliasSize pos lval (Nothing:size) (Just (a:altSize)) = 
  do sizeInt <- evalAliasSize pos lval size $ Just altSize
     return $ a:sizeInt
evalAliasSize pos lval ((Just s):size) altSize = 
  do aliasExpr lval s
     sizeVal <- evalExpr lval s >>= numberToModular
     sizeInt <- checkTypeInt pos sizeVal
     unless (sizeInt > 0) $
       pos <!!> arraySize
     sizeRest <- evalAliasSize pos lval size $ liftM tail altSize
     return $ sizeInt:sizeRest
evalAliasSize pos _ _ _ = pos <!!> arraySize

---- END

evalMain :: ProcMain -> Eval ()
evalMain proc@(ProcMain vdecls body pos) =
  do mapM_ initBinding vdecls
     whenDebugging ((liftIO $ putStrLn "Welcome to the Jana debugger. Type \"h[elp]\" for the help menu.") >> makeBreak)
     evalStmts body
  where 
    initBinding (Scalar (Int _)   id Nothing     _)   = bindVar id $ JInt 0
    initBinding (Scalar (Stack _) id Nothing     _)   = bindVar id nil
    initBinding (Scalar typ       id (Just expr) pos) = 
      do val <- evalModularAliasExpr (Var id) expr
         checkType typ val
         bindVar id val
    initBinding (Array id [Nothing]     Nothing pos) = pos <!!> arraySizeMissing id
    initBinding (Array id size Nothing pos) = 
      do sizeInt <- evalSize pos size Nothing
         exprs <- flattenArray pos sizeInt $ ArrayE [] pos
         vals  <- mapM evalModularExpr exprs
         valsI <- mapM (checkTypeInt pos) vals
         bindVar id $ JArray sizeInt valsI
    initBinding (Array id size (Just expr) pos) = 
      do sizeInt <- evalSize pos size $ Just $ sizeEstimate expr
         exprs <- flattenArray pos sizeInt expr
         vals  <- mapM evalModularExpr exprs
         valsI <- mapM (checkTypeInt pos) vals
         bindVar id $ JArray sizeInt valsI
        

evalProc :: Proc -> [Ident] -> Eval ()
evalProc proc args = inProcedure proc $
  do checkNumArgs (length vdecls) (length args)
     checkArgTypes
     oldStore <- getStore
     newStore <- localStore
     putStore newStore
     local updateAliases (evalStmts $ body proc)
     putStore oldStore
  where vdecls = params proc
        refs = mapM getRef args
        procPos Proc { procname = Ident _ pos } = pos
        checkNumArgs expArgs gotArgs =
          when (expArgs /= gotArgs) $
            procPos proc <!!> argumentError proc expArgs gotArgs
        checkArg (vdecl, ref) = inArgument (ident proc) (ident $ getVdeclIdent vdecl) $
          getRefValue ref >>= checkVdecl vdecl
        checkArgTypes =
          liftM (zip vdecls) refs >>= mapM_ checkArg
        localStore =
          liftM (storeFromList . zip (map (ident . getVdeclIdent) vdecls)) refs
        getVdeclIdent (Scalar _ id _ _) = id
        getVdeclIdent (Array id _ _ _)  = id
        updateAliases env =
          let xs = zip (map ident args) (map ident vdecls) in
            env { aliases = introAndPropAliases xs (aliases env) }


assignLval :: ModOp -> Lval -> Expr -> SourcePos -> Eval ()
assignLval modOp lv@(Var id) expr _ =
  do exprVal <- evalModularAliasExpr lv expr
     varVal  <- getVar id
     performModOperation modOp varVal exprVal exprPos exprPos >>= setVar id
  where exprPos = getExprPos expr
assignLval modOp (Lookup id idxExpr) expr pos =
  do let ps      = map getExprPos idxExpr
     idx         <- mapM (\(e, p) -> (unpackInt p =<< evalModularAliasExpr (Var id) e)) $ zip idxExpr ps 
     (sIdx,arr)  <- unpackArray pos =<< getVar id
     let idxVals = map (\(i,p) -> Number i p) $ zip idx ps
     val    <- evalModularAliasExpr (Lookup id idxVals) expr
     oldval <- arrayLookup (sIdx,arr) idx pos
     newval <- unpackInt pos =<< performModOperation modOp oldval val exprPos exprPos
     arrUpd <- arrayModify pos sIdx arr idx newval
     setVar id $ JArray sIdx arrUpd
  where exprPos = getExprPos expr

evalStmts :: [Stmt] -> Eval ()
evalStmts = mapM_ (\stmt -> inStatement stmt $ evalStmt stmt)

makeBreak :: Eval ()
makeBreak =
  whenDebugging ((parseDBCommand . splitArgs) =<< (liftIO $ getLine))
  where 
    splitArgs s = 
      case dropWhile isSpace s of
        "" -> []
        s' -> w : splitArgs s''
          where (w, s'') = break isSpace s'


parseDBCommand :: [String] -> Eval ()
parseDBCommand ("a":n)      = mapM (addBreakPoint . read) n >> makeBreak
parseDBCommand ("add":n)    = parseDBCommand ("a":n)
parseDBCommand ["c"]        = executeForward
parseDBCommand ["continue"] = parseDBCommand ["c"]
parseDBCommand ("d":n)      = mapM (removeBreakPoint . read) n >> makeBreak
parseDBCommand ("delete":n) = parseDBCommand ("d":n)
parseDBCommand ["h"]        = (liftIO $ putStrLn dbUsage) >> makeBreak
parseDBCommand ["help"]     = parseDBCommand ["h"]
parseDBCommand ("p":var)    = mapM printVar var >> makeBreak
  where 
    printVar var =
     do val <- getVar $ Ident var $ newPos "" 0 0
        liftIO $ putStrLn $ printVdecl var val 
parseDBCommand ("print":v)  = parseDBCommand ("p":v)
parseDBCommand ["r"]        = executeBackward
parseDBCommand ["reverse"] = parseDBCommand ["r"]
parseDBCommand ["s"]        = 
  do env <- get
     liftIO $ showStore env >>= putStrLn
     makeBreak
parseDBCommand ["store"]    = parseDBCommand ["s"]
parseDBCommand str          = (liftIO $ putStrLn errorTxt) >> makeBreak
  where 
    errorTxt = "Unknown command: \"" ++ (intercalate " " str) ++ "\". Type \"h[elp]\" to see known commands."

dbUsage = "Usage of the jana debugger\n\
        \IMPORTANT: all breakpoints will be added at the beginning of a line and only on statements.\n\
        \options:\n\
        \  a[dd] N*     adds zero or more breakpoint at lines N (space separated) \n\
        \  c[ontinue]   continues execution to next breakpoint or the end\n\
        \  d[elete] N*  deletes zero or more breakpoints at lines N (space separated)\n\
        \  h[elp]       this menu\n\
        \  p[rint] V*   prints the content of variables V (space separated)\n\
        \  r[everse]    reverse execution to precious breakpoint\n\
        \  s[tore]      prints entire store"


evalStmt :: Stmt -> Eval ()
evalStmt (Debug pos) = 
  do 
    isBreak <- checkForBreak pos
    when isBreak $ 
      (liftIO $ putStrLn $ "[Break at line " ++ (show $ sourceLine pos) ++ "] ") >> makeBreak

evalStmt (Assign modOp lval expr pos) = assignLval modOp lval expr pos
evalStmt (If e1 s1 s2 e2 _) =
  do val1 <- unpackBool (getExprPos e1) =<< evalModularExpr e1
     if val1
       then do evalStmts s1
               assertTrue e2
       else do evalStmts s2
               assertFalse e2
evalStmt (From e1 s1 s2 e2 _) =
  do assertTrue e1
     evalStmts s1
     loop
  where loop = do val <- unpackBool (getExprPos e2) =<< evalModularExpr e2
                  unless val loopRec
        loopRec = do evalStmts s2
                     assertFalse e1
                     evalStmts s1
                     loop
evalStmt (Push id1 id2 pos) =
  do head <- unpackInt pos   =<< getVar id1
     tail <- unpackStack pos =<< getVar id2
     setVar id2 $ JStack $ head : tail
     setVar id1 $ JInt 0
evalStmt stmt@(Pop id1 id2 pos) =
  do head <- unpackInt pos   =<< getVar id1
     tail <- unpackStack pos =<< getVar id2
     if head /= 0
       then pos <!!> popToNonZero id1
       else case tail of
         (x:xs) -> setVar id1 (JInt x) >> setVar id2 (JStack xs)
         []     -> pos <!!> emptyStack
evalStmt (Local assign1 stmts assign2 _) =
  do checkIdentAndType assign1 assign2
     createBinding assign1
     evalStmts stmts
     assertBinding assign2
  where createBinding (LocalVar typ id expr pos) =
          do val <- evalModularExpr expr
             checkType typ val
             bindVar id val
        createBinding (LocalArray id size expr pos) =
          do sizeInt <- evalSize pos size $ Just $ sizeEstimate expr
             exprs <- flattenArray pos sizeInt expr
             vals  <- mapM evalModularExpr exprs
             valsI <- mapM (checkTypeInt pos) vals
             bindVar id $ JArray sizeInt valsI
        assertBinding (LocalVar _ id expr pos) =
          do val <- evalModularAliasExpr (Var id) expr
             val' <- getVar id
             unless (val == val') $
               pos <!!> wrongDelocalValue id (show val) (show val')
             unbindVar id
        assertBinding (LocalArray id size expr pos) =
          do sizeInt <- evalAliasSize pos (Just (Var id)) size $ Just $ sizeEstimate expr
             exprs <- flattenArray pos sizeInt expr
             vals  <- mapM (evalModularAliasExpr (Var id)) exprs
             valsI <- mapM (checkTypeInt pos) vals
             let valsC = JArray sizeInt valsI
             vals' <- getVar id
             unless (valsC == vals') $
               pos <!!> wrongDelocalValue id (show valsC) (show vals')
             unbindVar id
        checkIdentAndType (LocalVar typ1 id1 _ _) (LocalVar typ2 id2 _ pos) =
          do unless (id1 == id2) $
               pos <!!> delocalNameMismatch id1 id2
             unless (typ1 == typ2) $
               pos <!!> delocalTypeMismatch id1 (show typ1) (show typ2)
        checkIdentAndType (LocalArray id1 size1 _ pos) (LocalArray id2 size2 _ _) =
          do unless (id1 == id2) $
               pos <!!> delocalNameMismatch id1 id2
        checkIdentAndType (LocalArray id1 _ _ pos) (LocalVar typ _ _ _) =
               pos <!!> delocalTypeMismatch id1 "Array" (show typ)
        checkIdentAndType (LocalVar typ _ _ _) (LocalArray id1 _ _ pos) =
               pos <!!> delocalTypeMismatch id1 "Array" (show typ)

evalStmt stmt@(Call funId args _) =
  do proc <- getProc funId
     evalProc proc args
evalStmt (Uncall funId args _) =
  do proc <- getProc funId
     evalProc (invertProc proc) args
evalStmt (Swap id1 id2 pos) =
  do alias <- id1 `isSameArrayElement` id2
     unless alias $ do
       val1 <- evalLval (Just id2) id1
       val2 <- evalLval (Just id1) id2
       if typesMatch val1 val2
         then setLval id2 val1 >> setLval id1 val2
         else pos <!!> swapTypeError (showValueType val1) (showValueType val2)
  where
    setLval (Var id) val = setVar id val
    setLval (Lookup id idxExpr) (JInt val) =
      do let ps = map getExprPos idxExpr
         idx <- mapM (\(e, p) -> (unpackInt p =<< evalModularExpr e)) $ zip idxExpr ps
         (sIdx,arr) <- unpackArray pos =<< getVar id
         arrUpd <- arrayModify (head ps) sIdx arr idx val
         setVar id $ JArray sIdx arrUpd
    setLval _ val = 
      pos <!!> swapTypeError (showValueType val) "array"
    Var x1 `isSameArrayElement` Var x2 = return False
    Lookup x1 e1 `isSameArrayElement` Lookup x2 e2 =
        do v1 <- mapM (evalExpr Nothing) e1
           v2 <- mapM (evalExpr Nothing) e2
           return $ x1 == x2 && v1 == v2
evalStmt (UserError msg pos)          = pos <!!> userError msg
evalStmt (Prints (Print msg) pos)     = liftIO $ putStrLn msg
evalStmt (Prints (Printf msg []) pos) = evalStmts [Prints (Print msg) pos]
evalStmt (Prints (Printf msg vars) pos) =
  do varList' <- varList
     case printfRender [msg] varList' of
       Right str -> liftIO $ putStrLn str
       Left  err -> pos <!!> err
  where varList =
          mapM makeVarPair vars
        makeVarPair var =
          do val <- getVar var
             return (show val, showValueType val)
evalStmt (Prints (Show vars) pos) =
  do strs <- mapM showVar vars
     liftIO $ putStrLn $ intercalate ", " strs
  where showVar :: Ident -> Eval String
        showVar var = liftM (printVdecl (ident var)) (getVar var)
evalStmt (Skip _) = return ()
evalStmt (Assert e pos) = assertTrue e

evalLval :: Maybe Lval -> Lval -> Eval Value
evalLval lv (Var id) = checkLvalAlias lv (Var id) >> getVar id
evalLval lv (Lookup id@(Ident _ pos) es) =
  do let ps = map getExprPos es
     idx <- mapM (\(e, p) -> (unpackInt p =<< evalModularExpr e)) $ zip es ps
     let idxVals = map (\(i,p) -> Number i p) $ zip idx ps
     checkLvalAlias lv (Lookup id idxVals)
     arr <- unpackArray pos =<< getVar id
     arrayLookup arr idx pos

numberToModular :: Value -> Eval Value
numberToModular (JInt x) =
  do flag <- asks (modInt . evalOptions)
     return $ JInt $ if flag then ((x + 2^31) `mod` 2^32) - 2^31 else x
numberToModular val = return val


evalModularExpr :: Expr -> Eval Value
evalModularExpr expr = evalExpr Nothing expr >>= numberToModular

evalModularAliasExpr :: Lval -> Expr -> Eval Value
evalModularAliasExpr lv expr = evalExpr (Just lv) expr >>= numberToModular

findAlias :: Ident -> Ident -> Eval ()
findAlias id1 id2@(Ident _ pos) =
  do aliasSet <- asks aliases
     when (isAlias aliasSet (ident id1) (ident id2)) $
       pos <!!> aliasError id1 id2

checkAlias :: Maybe Lval -> Ident -> Eval ()
checkAlias Nothing _ = return ()
checkAlias (Just (Var id)) id2 = findAlias id id2
checkAlias (Just (Lookup id _)) id2 = findAlias id id2

checkLvalAlias :: Maybe Lval -> Lval -> Eval ()
checkLvalAlias Nothing _ = return ()
checkLvalAlias (Just (Var id)) (Var id2) = findAlias id id2
checkLvalAlias (Just (Var id)) (Lookup id2 _) = findAlias id id2
checkLvalAlias (Just (Lookup id _)) (Var id2) = findAlias id id2
checkLvalAlias (Just (Lookup id exprn)) (Lookup id2 exprm) =
  do n <- mapM (evalExpr Nothing) exprn
     m <- mapM (evalExpr Nothing) exprm
     if   n == m 
       then findAlias id id2
       else return ()

evalExpr :: Maybe Lval -> Expr -> Eval Value
evalExpr _ (Number x _)       = return $ JInt x
evalExpr _ (Boolean b _)      = return $ JBool b
evalExpr _ (Nil _)            = return nil
evalExpr lv expr@(LV val _)   = inExpression expr $ evalLval lv val
evalExpr lv expr@(UnaryOp Not e) = inExpression expr $
  do x <- unpackBool (getExprPos e) =<< evalExpr lv e
     return $ JBool $ not x
evalExpr lv expr@(BinOp LAnd e1 e2) = inExpression expr $
  do x <- unpackBool (getExprPos e1) =<< evalExpr lv e1
     if x
       then liftM JBool (unpackBool (getExprPos e2) =<< evalExpr lv e2)
       else return $ JBool False
evalExpr lv expr@(BinOp LOr e1 e2) = inExpression expr $
  do x <- unpackBool (getExprPos e1) =<< evalExpr lv e1
     if x
       then return $ JBool True
       else liftM JBool $ unpackBool (getExprPos e2) =<< evalExpr lv e2
evalExpr lv expr@(BinOp op e1 e2) = inExpression expr $
  do x <- evalExpr lv e1
     y <- evalExpr lv e2
     performOperation op x y (getExprPos e1) (getExprPos e2)
evalExpr lv expr@(Top id pos) = inArgument "top" (ident id) $
  do checkAlias lv id
     stack <- unpackStack pos =<< getVar id
     case stack of
       (x:xs) -> return $ JInt x
       []     -> return nil
evalExpr lv expr@(Empty id pos) = inArgument "empty" (ident id) $
  do checkAlias lv id
     stack <- unpackStack pos =<< getVar id
     case stack of
       [] -> return $ JBool True
       _  -> return $ JBool False
evalExpr _ expr@(Size id@(Ident _ pos) _) = inArgument "size" (ident id) $
  do boxedVal <- getVar id
     case boxedVal of
       JArray (i:_) _ -> return $ JInt (toInteger i)
       JStack xs -> return $ JInt (toInteger $ length xs)
       val       -> pos <!!> typeMismatch ["array", "stack"] (showValueType val)

-- |This alias check differs to the alias check in the evalExpr in that it also includes size.
-- |This is used in delocal where the delocaliser array is not allowed.
aliasExpr :: Maybe Lval -> Expr -> Eval ()
aliasExpr _  (Number _ _)    = return ()
aliasExpr _  (Boolean _ _)   = return ()
aliasExpr _  (Nil _)         = return ()
aliasExpr lv (LV val _)      = checkLvalAlias lv val
aliasExpr lv (UnaryOp Not e) = aliasExpr lv e
aliasExpr lv (BinOp _ e1 e2) = aliasExpr lv e1 >> aliasExpr lv e2
aliasExpr lv (Top id pos)    = checkAlias lv id
aliasExpr lv (Empty id pos)  = checkAlias lv id
aliasExpr lv (Size id _)     = checkAlias lv id
