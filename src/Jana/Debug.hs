module Jana.Debug where

import Jana.Ast

injectDBProgram :: Program -> Program
injectDBProgram (Program mains procs) =
  Program (map injectDBProcMain mains) (map injectDBProc procs)


injectDBProcMain :: ProcMain -> ProcMain
injectDBProcMain (ProcMain vdecls stmts sourcePos) =
  ProcMain vdecls debugStmts sourcePos
  where
    injStmts = injectDBStmts stmts
    debugStmts = ((changeDebug $ head injStmts):(tail $ injectDBStmts stmts)) ++ [Debug End (stmtPos $ last stmts)]
    changeDebug (Debug _ pos) = Debug Beginning pos


injectDBProc :: Proc -> Proc
injectDBProc proc = proc { body = injectDBStmts $ body proc }

injectDBStmts :: [Stmt] -> [Stmt]
injectDBStmts = concatMap injectDBStmt

injectDBStmt :: Stmt -> [Stmt]
injectDBStmt (If e1 ifPart elsePart e2 pos) =
  [Debug Normal pos, If e1 (injectDBStmts ifPart) (injectDBStmts elsePart) e2 pos]
injectDBStmt (From e1 doPart loopPart e2 pos) =
  [Debug Normal pos, From e1 (injectDBStmts doPart) (injectDBStmts loopPart) e2 pos]
injectDBStmt (Local assign1 body assign2 pos) =
  [Debug Normal pos, Local assign1 (injectDBStmts body) assign2 pos]
injectDBStmt stmt = [Debug Normal (stmtPos stmt), stmt]

