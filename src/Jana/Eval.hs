module Jana.Eval (
  runProgram,
  evalLval,
  evalExpr,
  runEval,
  ) where


import Prelude hiding (GT, LT, EQ, userError)
import System.Exit
import System.IO
import Data.Char (toLower, isSpace)
import Data.List (genericSplitAt, intercalate, genericTake,  genericIndex)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import Text.Parsec.Pos

import Jana.Aliases
import Jana.Ast
import Jana.Types
import Jana.Invert
import Jana.Error
import Jana.ErrorMessages
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
     unless (val1 == bool) $ pos <!!> msg
  where
    msg = assertionFail ("should be " ++ map toLower (show bool))
    pos = getExprPos expr

assertTrue :: Expr -> Eval ()
assertTrue  expr = catchError (assert True  expr) catchDebugError

assertFalse :: Expr -> Eval ()
assertFalse expr = catchError (assert False expr) catchDebugError

checkType :: Type -> Value -> Eval ()
checkType (Int _)     (JInt _)     = return ()
checkType (Int _)     (JArray _ _) = return ()
checkType (BoolT _)   (JBool _)    = return ()
checkType (Stack _)   (JStack _)   = return ()
checkType (Int pos)   val = pos <!!> typeMismatch ["int"] (showValueType val)
checkType (Stack pos) val = pos <!!> typeMismatch ["stack"] (showValueType val)
checkType (BoolT pos) val = pos <!!> typeMismatch ["bool"] (showValueType val)

checkTypeInt :: SourcePos -> Value -> Eval Integer
checkTypeInt _   (JInt v) = return v
checkTypeInt pos val      = pos <!!> typeMismatch ["int"] (showValueType val)

-- This is likely to be wrong
checkVdecl :: Vdecl -> Value -> Eval ()
checkVdecl (Scalar Int {}   _ _ _)  (JInt _)     = return ()
checkVdecl (Scalar Stack {} _ _ _)  (JStack _)   = return ()
checkVdecl (Array _ []  _ _)   (JArray _ _) = return ()
checkVdecl (Array _ sizeExpr _ pos) (JArray size _) =
  do _ <- evalSize pos sizeExpr (Just size)
     return ()
     -- val <- evalModularExpr x
     -- valInt <- checkTypeInt pos val
     -- unless (valInt == arrLen) $ pos <!!> arraySizeMismatch valInt arrLen
  -- where arrLen = toInteger (length arr)
checkVdecl (StackD _ _ _)   (JStack _) = return ()
checkVdecl vdecl val =
  vdeclPos vdecl <!!> typeMismatch [vdeclType vdecl] (showValueType val)
  where vdeclPos (Scalar _ _ _ pos) = pos
        vdeclPos (Array _ _ _ pos)  = pos
        vdeclPos (StackD _ _ pos)   = pos
        vdeclType (Scalar Int{} _ _ _)   = "int"
        vdeclType (Scalar BoolT{} _ _ _) = "bool"
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
getExprPos (ArrayE _ pos)  = pos

runProgram :: String -> Program -> EvalOptions -> IO ()
runProgram _ p@(Program (Just _) _) evalOpts =
  runProgramAfterDBcheck checkDB evalOpts
  where
    checkDB =
      if (runDebugger evalOpts) == DebugOff
        then p
        else injectDBProgram p
runProgram filename (Program Nothing _) _ =
  print (newFileError filename noMainProc) >> exitWith (ExitFailure 1)

runProgramAfterDBcheck :: Program -> EvalOptions -> IO ()
runProgramAfterDBcheck (Program (Just main) procs) evalOpts =
  case procEnvFromList procs of
    Left err -> print err >> exitWith (ExitFailure 1)
    Right pEnv ->
      let env = EE { procEnv = pEnv
                   , evalOptions = evalOpts
                   , aliases = Jana.Aliases.empty}
      in do runRes <- runEval (evalMain main) emptyStore env
            case runRes of
              Right (_, s) -> showStore s >>= putStrLn
              Left err     -> print err >> exitWith (ExitFailure 1)
runProgramAfterDBcheck _ _ = error ""

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
     sizeVal <- evalModularAliasExpr lval s
     sizeInt <- checkTypeInt pos sizeVal
     unless (sizeInt > 0) $
       pos <!!> arraySize
     sizeRest <- evalAliasSize pos lval size $ liftM tail altSize
     return $ sizeInt:sizeRest
evalAliasSize pos _ _ _ = pos <!!> arraySize

---- END

evalMain :: ProcMain -> Eval ()
evalMain (ProcMain vdecls mainbody _) =
  do mapM_ initBinding vdecls
     evalStmts mainbody
  where
    initBinding (Scalar (BoolT _) idnt Nothing     _)   = bindVar idnt $ JBool True
    initBinding (Scalar (Int _)   idnt Nothing     _)   = bindVar idnt $ JInt 0
    initBinding (Scalar (Stack _) idnt Nothing     _)   = bindVar idnt nil
    initBinding (Scalar typ       idnt (Just expr) _) =
      do val <- evalModularAliasExpr (Just $ Var idnt) expr
         checkType typ val
         bindVar idnt val
    initBinding (Array idnt [Nothing]     Nothing pos) = pos <!!> arraySizeMissing idnt
    initBinding (Array idnt size Nothing pos) =
      do sizeInt <- evalSize pos size Nothing
         exprs <- flattenArray pos sizeInt $ ArrayE [] pos
         vals  <- mapM evalModularExpr exprs
         valsI <- mapM (checkTypeInt pos) vals
         bindVar idnt $ JArray sizeInt valsI
    initBinding (Array idnt size (Just expr) pos) =
      do sizeInt <- evalSize pos size $ Just $ sizeEstimate expr
         exprs <- flattenArray pos sizeInt expr
         vals  <- mapM evalModularExpr exprs
         valsI <- mapM (checkTypeInt pos) vals
         bindVar idnt $ JArray sizeInt valsI
    initBinding (StackD idnt Nothing pos) = bindVar idnt nil
    initBinding (StackD idnt (Just (Nil _)) pos) = bindVar idnt $ JStack []
    initBinding (StackD idnt (Just (ArrayE exprs _)) pos) =
      do vals  <- mapM evalModularExpr exprs
         valsI <- mapM (checkTypeInt pos) vals
         bindVar idnt $ JStack valsI

evalProc :: Proc -> [Ident] -> Eval ()
evalProc proc args = inProcedure proc $
  do checkNumArgs (length vdecls) (length args)
     checkArgTypes
     oldStore <- getStore
     newStore <- localStore
     putStore newStore
     local updateAliases (evalStmts $ body proc)
     putStore oldStore
  where
    vdecls = params proc
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
    getVdeclIdent (Scalar _ idnt _ _) = idnt
    getVdeclIdent (Array idnt _ _ _)  = idnt
    updateAliases env =
      let xs = zip (map ident args) (map ident vdecls) in
        env { aliases = introAndPropAliases xs (aliases env) }

assignLval :: ModOp -> Lval -> Expr -> SourcePos -> Eval ()
assignLval modOp lv@(Var idnt) expr _ =
  do exprVal <- evalModularAliasExpr (Just lv) expr
     varVal  <- getVar idnt
     val <- performModOperation modOp varVal exprVal exprPos exprPos >>= numberToModular
     setVar idnt val
  where exprPos = getExprPos expr
assignLval modOp (Lookup idnt idxExpr) expr pos =
  do let ps      = map getExprPos idxExpr
     idx         <- mapM (\(e, p) -> (unpackInt p =<< evalModularAliasExpr (Just $ Var idnt) e)) $ zip idxExpr ps 
     (sIdx,arr)  <- unpackArray pos =<< getVar idnt
     let idxVals = map (\(i,p) -> Number i p) $ zip idx ps
     val    <- evalModularAliasExpr (Just $ Lookup idnt idxVals) expr
     oldval <- arrayLookup (sIdx,arr) idx pos
     newval <- unpackInt pos =<< numberToModular =<< performModOperation modOp oldval val exprPos exprPos
     arrUpd <- arrayModify pos sIdx arr idx newval
     setVar idnt $ JArray sIdx arrUpd
  where exprPos = getExprPos expr

evalStmts :: [Stmt] -> Eval ()
evalStmts stmts =
  whenForwardExecutionElse
    (evalStmts_ stmts [])
    (evalStmts_ [] (reverse stmts))

evalStmts_ :: [Stmt] -> [Stmt] -> Eval ()
evalStmts_ s_come s_done =
  whenForwardExecutionElse (fwd s_come) (bck s_done)
  where
    fwd []     = return ()
    fwd (s:sc) = evalStmtFwd s >> whenForwardExecutionElse (evalStmts_ sc (s:s_done)) (evalStmts_ s_come s_done)
    bck []     = return ()
    bck (s:sd) = evalStmtBck s >> whenForwardExecutionElse (evalStmts_ (s_come) s_done) (evalStmts_ (s:s_come) sd)

evalStmtBck :: Stmt -> Eval ()
evalStmtBck stmt =
  flipExecution >> (catchError (inStatement invStmt $ evalStmt invStmt) catchDebugError) >> flipExecution
  where
    invStmt = invertStmt Locally stmt

evalStmtFwd :: Stmt -> Eval ()
evalStmtFwd stmt =
  catchError (inStatement stmt $ evalStmt stmt) catchDebugError

makeBreak :: SourcePos -> Eval ()
makeBreak pos =
  whenDebugging debug
  where
    debug = do
      liftIO $ (putStr "> ") >> hFlush stdout
      c <- liftIO getLine
      parseDBCommand pos $ splitArgs c
    splitArgs s =
      case dropWhile isSpace s of
        "" -> []
        s' -> w : splitArgs s''
          where (w, s'') = break isSpace s'

parseDBCommand :: SourcePos -> [String] -> Eval ()
parseDBCommand pos ("a":n)      = mapM (addBreakPoint . read) n >> makeBreak pos
parseDBCommand pos ("add":n)    = parseDBCommand pos ("a":n)
parseDBCommand _   ["b"]        = executeBackward
parseDBCommand pos ["backward"] = parseDBCommand pos ["b"]
parseDBCommand pos ("d":n)      = mapM (removeBreakPoint . read) n >> makeBreak pos
parseDBCommand pos ("delete":n) = parseDBCommand pos ("d":n)
parseDBCommand _   ["f"]        = executeForward
parseDBCommand pos ["forward"]  = parseDBCommand pos ["f"]
parseDBCommand pos ["h"]        = (liftIO $ putStrLn dbUsage) >> makeBreak pos
parseDBCommand pos ["help"]     = parseDBCommand pos ["h"]
parseDBCommand pos ["l"]        = (liftIO $ putStrLn ("[Current line is " ++ (show $ sourceLine pos) ++ "]")) >> makeBreak pos
parseDBCommand pos ["line"]     = parseDBCommand pos ["l"]
parseDBCommand pos ("p":var)    = mapM (\x -> catchError (printVar x) catchDebugError) var >> makeBreak pos
  where
    printVar v =
     do val <- (getVar $ Ident v pos)
        liftIO $ putStrLn $ printVdecl v val
parseDBCommand pos ("print":v)  = parseDBCommand pos ("p":v)
parseDBCommand pos ["s"]        =
  do env <- get
     liftIO $ showStore env >>= putStrLn
     makeBreak pos
parseDBCommand pos ["store"]    = parseDBCommand pos ["s"]
parseDBCommand _   ["q"]        = liftIO $ exitWith $ ExitSuccess
parseDBCommand pos ["quit"]     = parseDBCommand pos ["q"]
parseDBCommand pos str          = (liftIO $ putStrLn errorTxt) >> makeBreak pos
  where
    errorTxt = "Unknown command: \"" ++ (intercalate " " str) ++ "\". Type \"h[elp]\" to see known commands."

catchDebugError :: JanaError  -> Eval ()
catchDebugError msg =
  whenDebugging
    (liftIO $ putStrLn $ "[Break: ERROR (line " ++ (show $ sourceLine $ errorPos msg) ++ ")]") >>
      (liftIO $ putStrLn $ show (errorMessages msg)) >>
      makeBreak (errorPos msg)

dbUsage :: String
dbUsage = "Usage of the jana debugger\n\
        \IMPORTANT: all breakpoints will be added at the beginning of a line and only on statements.\n\
        \options:\n\
        \  a[dd] N*     adds zero or more breakpoint at lines N (space separated) \n\
        \  b[ackward]   reverse execution to previous breakpoint\n\
        \  d[elete] N*  deletes zero or more breakpoints at lines N (space separated)\n\
        \  f[orward]    execution to next breakpoint in forward direction\n\
        \  h[elp]       this menu\n\
        \  l[ine]       print current line\n\
        \  p[rint] V*   prints the content of variables V (space separated)\n\
        \  s[tore]      prints entire store\n\
        \  q[uit]       quit the debugger (ends termination)"


evalStmt :: Stmt -> Eval ()
-- evalStmt stmt | trace ("EvalStmt at line" ++ (show $ sourceLine $ stmtPos stmt) ++ " doing " ++ (show stmt)) False = undefined
evalStmt (Debug Beginning pos) =
  do whenFullDebugging $
       whenFirstBreak
         (liftIO $ putStrLn "Welcome to the Jana debugger. Type \"h[elp]\" for the help menu.")
         (liftIO $ putStrLn $ "[Break at BEGIN (line " ++ (show $ sourceLine pos) ++ ")]") >>
        makeBreak pos
     er <- isErrorDebugging
     fw <- isUserForwardExecution
     when (er && (not fw)) ((liftIO $ putStrLn $ "[Break at BEGIN (line " ++ (show $ sourceLine pos) ++ ")]") >> makeBreak pos)
evalStmt (Debug End pos) =
  whenFullDebugging $ (liftIO $ putStrLn $ "[Break at END (_after_ line " ++ (show $ sourceLine pos) ++ ")]") >> makeBreak pos
evalStmt (Debug Normal pos) =
  do isBreak <- checkForBreak pos
     when isBreak $
       (liftIO $ putStrLn $ "[Break at line " ++ (show $ sourceLine pos) ++ "] ") >> makeBreak pos
evalStmt (Assign modOp lval expr pos) = assignLval modOp lval expr pos
evalStmt (If e1 s1 s2 e2 _) =
  do val1 <- unpackBool (getExprPos e1) =<< evalModularExpr e1
     if val1
       then do evalStmts s1
               whenForwardExecution  (assertTrue e2 >> whenBackwardExecution (evalStmts s1))
               whenBackwardExecution (assertTrue e1)
       else do evalStmts s2
               whenForwardExecution  (assertFalse e2 >> whenBackwardExecution (evalStmts s2))
               whenBackwardExecution (assertFalse e1)
evalStmt (From e1 s1 s2 e2 _) =
  do assertTrue e1
     whenForwardExecution loop
  where loop = do evalStmts s1
                  val <- unpackBool (getExprPos e2) =<< evalModularExpr e2
                  whenForwardExecution (unless val (loopRec >> whenBackwardExecution (assertFalse e2 >> evalStmts s1)))
        loopRec = do evalStmts s2
                     whenForwardExecution
                       (assertFalse e1 >>
                         whenForwardExecution (loop >> whenBackwardExecution (assertFalse e1)) >>
                         whenBackwardExecution (evalStmts s2))
evalStmt (Iterate typ idnt startE stepE endE stmts pos) =
  evalStmt (Local
    (LocalVar typ idnt startE pos)
    [From
      (BinOp EQ (LV (Var idnt) pos) startE)
      []
      (stmts++[Assign AddEq (Var idnt) stepE pos])
      (BinOp EQ (LV (Var idnt) pos) (BinOp Add endE stepE)) pos]
    (LocalVar typ idnt (BinOp Add endE stepE) pos) pos)
evalStmt (Push idnt1 idnt2 pos) =
  do stkhead <- unpackInt pos   =<< getVar idnt1
     stktail <- unpackStack pos =<< getVar idnt2
     setVar idnt2 $ JStack $ stkhead : stktail
     setVar idnt1 $ JInt 0
evalStmt (Pop idnt1 idnt2 pos) =
  do stkhead <- unpackInt pos   =<< getVar idnt1
     stktail <- unpackStack pos =<< getVar idnt2
     if stkhead /= 0
       then pos <!!> popToNonZero idnt1
       else case stktail of
         (x:xs) -> setVar idnt1 (JInt x) >> setVar idnt2 (JStack xs)
         []     -> pos <!!> emptyStack
evalStmt (Local assign1 stmts assign2 _) =
  do checkIdentAndType assign1 assign2
     createBinding assign1
     evalStmts stmts
     whenForwardExecutionElse (assertBinding assign2) (assertBinding assign1)
  where
    createBinding (LocalVar typ idnt expr _) =
      do val <- evalModularExpr expr
         checkType typ val
         bindVar idnt val
    createBinding (LocalArray idnt size expr pos) =
      do sizeInt <- evalSize pos size $ Just $ sizeEstimate expr
         exprs <- flattenArray pos sizeInt expr
         vals  <- mapM evalModularExpr exprs
         valsI <- mapM (checkTypeInt pos) vals
         bindVar idnt $ JArray sizeInt valsI
    assertBinding (LocalVar _ idnt expr pos) =
      do val <- evalModularAliasExpr (Just $ Var idnt) expr
         val' <- getVar idnt
         unless (val == val') $
           pos <!!> wrongDelocalValue idnt (show val) (show val')
         unbindVar idnt
    assertBinding (LocalArray idnt size expr pos) =
      do sizeInt <- evalAliasSize pos (Just (Var idnt)) size $ Just $ sizeEstimate expr
         exprs <- flattenArray pos sizeInt expr
         vals  <- mapM (evalModularAliasExpr (Just $ Var idnt)) exprs
         valsI <- mapM (checkTypeInt pos) vals
         let valsC = JArray sizeInt valsI
         vals' <- getVar idnt
         unless (valsC == vals') $
           pos <!!> wrongDelocalValue idnt (show valsC) (show vals')
         unbindVar idnt
    checkIdentAndType (LocalVar typ1 id1 _ _) (LocalVar typ2 id2 _ pos) =
      do unless (id1 == id2) $
           pos <!!> delocalNameMismatch id1 id2
         unless (typ1 == typ2) $
           pos <!!> delocalTypeMismatch id1 (show typ1) (show typ2)
    checkIdentAndType (LocalArray id1 _ _ pos) (LocalArray id2 _ _ _) =
      do unless (id1 == id2) $
           pos <!!> delocalNameMismatch id1 id2
    checkIdentAndType (LocalArray id1 _ _ pos) (LocalVar typ _ _ _) =
           pos <!!> delocalTypeMismatch id1 "Array" (show typ)
    checkIdentAndType (LocalVar typ _ _ _) (LocalArray id1 _ _ pos) =
           pos <!!> delocalTypeMismatch id1 "Array" (show typ)

evalStmt (Call funId args _) =
  do proc <- getProc funId
     evalProc proc args
evalStmt (Uncall funId args _) =
  do proc <- getProc funId
     evalProc (invertProc proc) args
evalStmt (ExtCall _ _ pos) =
  pos <!!> noExternalCalls
evalStmt (ExtUncall _ _ pos) =
  pos <!!> noExternalCalls
evalStmt (Swap id1 id2 pos) =
  do alias <- id1 `isSameArrayElement` id2
     unless alias $ do
       val1 <- evalLval (Just id2) id1
       val2 <- evalLval (Just id1) id2
       if typesMatch val1 val2
         then setLval id2 val1 >> setLval id1 val2
         else pos <!!> swapTypeError (showValueType val1) (showValueType val2)
  where
    setLval (Var idnt) val = setVar idnt val
    setLval (Lookup idnt idxExpr) (JInt val) =
      do let ps = map getExprPos idxExpr
         idx <- mapM (\(e, p) -> (unpackInt p =<< evalModularExpr e)) $ zip idxExpr ps
         (sIdx,arr) <- unpackArray pos =<< getVar idnt
         arrUpd <- arrayModify (head ps) sIdx arr idx val
         setVar idnt $ JArray sIdx arrUpd
    setLval _ val = 
      pos <!!> swapTypeError (showValueType val) "array"
    Var _ `isSameArrayElement` Var _ = return False
    Lookup x1 e1 `isSameArrayElement` Lookup x2 e2 =
        do v1 <- mapM (evalModularExpr) e1
           v2 <- mapM (evalModularExpr) e2
           return $ x1 == x2 && v1 == v2
    _ `isSameArrayElement` _ = error "Comparison of different types"
evalStmt (UserError msg pos)          = pos <!!> userError msg
evalStmt (Prints (Print msg) _)       = liftIO $ putStrLn msg
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
evalStmt (Prints (Show vars) _) =
  do strs <- mapM showVar vars
     liftIO $ putStrLn $ intercalate ", " strs
  where showVar :: Ident -> Eval String
        showVar var = liftM (printVdecl (ident var)) (getVar var)
evalStmt (Skip _) = return ()
evalStmt (Assert e _) = assertTrue e

evalLval :: Maybe Lval -> Lval -> Eval Value
evalLval lv (Var idnt) = checkLvalAlias lv (Var idnt) >> getVar idnt
evalLval lv (Lookup idnt@(Ident _ pos) es) =
  do let ps = map getExprPos es
     idx <- mapM (\(e, p) -> (unpackInt p =<< evalModularExpr e)) $ zip es ps
     let idxVals = map (\(i,p) -> Number i p) $ zip idx ps
     checkLvalAlias lv (Lookup idnt idxVals)
     arr <- unpackArray pos =<< getVar idnt
     arrayLookup arr idx pos

numberToModular :: Value -> Eval Value
numberToModular (JInt x) =
  do flag <- asks (modInt . evalOptions)
     return $ JInt $ ntm flag
  where
    ntm None     = x
    -- ntm ModPow32 = ((x + 2^7) `mod` 2^8) - 2^7
    ntm (ModPow2 n) = ((x + 2^(n-1)) `mod` 2^n) - 2^(n-1)
    ntm (ModPrime n) = x `mod` (toInteger n)
numberToModular val = return val


evalModularExpr :: Expr -> Eval Value
evalModularExpr expr = evalExpr Nothing expr >>= numberToModular

evalModularAliasExpr :: Maybe Lval -> Expr -> Eval Value
evalModularAliasExpr lv expr = evalExpr lv expr >>= numberToModular

findAlias :: Ident -> Ident -> Eval ()
findAlias idnt1 idnt2@(Ident _ pos) =
  do aliasSet <- asks aliases
     when (isAlias aliasSet (ident idnt1) (ident idnt2)) $
       pos <!!> aliasError idnt1 idnt2

checkAlias :: Maybe Lval -> Ident -> Eval ()
checkAlias Nothing _ = return ()
checkAlias (Just (Var idnt)) idnt2 = findAlias idnt idnt2
checkAlias (Just (Lookup idnt _)) idnt2 = findAlias idnt idnt2

checkLvalAlias :: Maybe Lval -> Lval -> Eval ()
checkLvalAlias Nothing _ = return ()
checkLvalAlias (Just (Var idnt)) (Var idnt2) = findAlias idnt idnt2
checkLvalAlias (Just (Var idnt)) (Lookup idnt2 _) = findAlias idnt idnt2
checkLvalAlias (Just (Lookup idnt _)) (Var idnt2) = findAlias idnt idnt2
checkLvalAlias (Just (Lookup idnt exprn)) (Lookup idnt2 exprm) =
  do n <- mapM (evalModularExpr) exprn
     m <- mapM (evalModularExpr) exprm
     if   n == m
       then findAlias idnt idnt2
       else return ()

evalExpr :: Maybe Lval -> Expr -> Eval Value
evalExpr _ (Number x _)       = return $ JInt x
evalExpr _ (Boolean b _)      = return $ JBool b
evalExpr _ (Nil _)            = return nil
evalExpr lv expr@(LV val _)   = inExpression expr $ evalLval lv val
evalExpr lv expr@(UnaryOp Not e) = inExpression expr $
  do x <- unpackBool (getExprPos e) =<< evalModularAliasExpr lv e
     return $ JBool $ not x
evalExpr lv expr@(BinOp LAnd e1 e2) = inExpression expr $
  do x <- unpackBool (getExprPos e1) =<< evalModularAliasExpr lv e1
     if x
       then liftM JBool (unpackBool (getExprPos e2) =<< evalModularAliasExpr lv e2)
       else return $ JBool False
evalExpr lv expr@(BinOp LOr e1 e2) = inExpression expr $
  do x <- unpackBool (getExprPos e1) =<< evalModularAliasExpr lv e1
     if x
       then return $ JBool True
       else liftM JBool $ unpackBool (getExprPos e2) =<< evalModularAliasExpr lv e2
evalExpr lv expr@(BinOp op e1 e2) = inExpression expr $
  do x <- evalModularAliasExpr lv e1
     y <- evalModularAliasExpr lv e2
     performOperation op x y (getExprPos e1) (getExprPos e2)
evalExpr lv (Top idnt pos) = inArgument "top" (ident idnt) $
  do checkAlias lv idnt
     stack <- unpackStack pos =<< getVar idnt
     case stack of
       (x:_) -> return $ JInt x
       []    -> return nil
evalExpr lv (Empty idnt pos) = inArgument "empty" (ident idnt) $
  do checkAlias lv idnt
     stack <- unpackStack pos =<< getVar idnt
     case stack of
       [] -> return $ JBool True
       _  -> return $ JBool False
evalExpr _ (Size idnt@(Ident _ pos) _) = inArgument "size" (ident idnt) $
  do boxedVal <- getVar idnt
     case boxedVal of
       JArray (i:_) _ -> return $ JInt (toInteger i)
       JStack xs -> return $ JInt (toInteger $ length xs)
       val       -> pos <!!> typeMismatch ["array", "stack"] (showValueType val)
evalExpr _ (UnaryOp FromLoop _) = error "Undefined evaluation of expression"
evalExpr _ (ArrayE _ _) = error "Undefined evaluation of expression"

-- |This alias check differs to the alias check in the evalExpr in that it also includes size.
-- |This is used in delocal where the delocaliser array is not allowed.
aliasExpr :: Maybe Lval -> Expr -> Eval ()
aliasExpr _  (Number _ _)         = return ()
aliasExpr _  (Boolean _ _)        = return ()
aliasExpr _  (Nil _)              = return ()
aliasExpr lv (LV val _)           = checkLvalAlias lv val
aliasExpr lv (UnaryOp Not e)      = aliasExpr lv e
aliasExpr lv (BinOp _ e1 e2)      = aliasExpr lv e1 >> aliasExpr lv e2
aliasExpr lv (Top idnt _)         = checkAlias lv idnt
aliasExpr lv (Empty idnt _)       = checkAlias lv idnt
aliasExpr lv (Size idnt _)        = checkAlias lv idnt
aliasExpr _  (UnaryOp FromLoop _) = error "FromLoop should have been extracted"
aliasExpr _  (ArrayE _ _)         = return ()

