module Jana.Invert where

import Prelude hiding (GT, LT, EQ, userError)

import Jana.Ast

data InvertMode = Globally | Locally

invertProgram :: Program -> Program
invertProgram (Program mains procs) =
  Program mains (map invertProcGlobally procs)

invertProc :: Proc -> Proc
invertProc proc = proc { body = invertStmts Locally $ body proc }

invertProcGlobally :: Proc -> Proc
invertProcGlobally proc = proc { body = invertStmts Globally $ body proc }

invertStmts :: InvertMode -> [Stmt] -> [Stmt]
invertStmts mode = reverse . map (invertStmt mode)

invertStmt :: InvertMode -> Stmt -> Stmt
invertStmt _ (Assign modOp lval expr pos) =
  Assign (invertModOp modOp) lval expr pos
  where invertModOp AddEq = SubEq
        invertModOp SubEq = AddEq
        invertModOp XorEq = XorEq
invertStmt mode (If e1 ifPart elsePart e2 pos) =
  If e2 (invertStmts mode ifPart) (invertStmts mode elsePart) e1 pos
invertStmt mode (From e1 doPart loopPart e2 pos) =
  From e2 (invertStmts mode doPart) (invertStmts mode loopPart) e1 pos
-- invertStmt mode (Iterate typ ident startE stepE endE stmts pos) =
--   invertStmt mode (Local
--     (LocalVar typ ident startE pos)
--     [From
--       (BinOp EQ (LV (Var ident) pos) startE)
--       []
--       (stmts++[Assign AddEq (Var ident) stepE pos])
--       (BinOp EQ (LV (Var ident) pos) endE) pos]
--     (LocalVar typ ident endE pos) pos)
invertStmt mode (Iterate typ ident startE stepE endE stmts pos) =
  Iterate typ ident
    endE
    (BinOp Sub (Number 0 pos) stepE)
    startE
    (invertStmts mode stmts)
    pos
invertStmt _ (Push id1 id2 pos) = Pop  id1 id2 pos
invertStmt _ (Pop  id1 id2 pos) = Push id1 id2 pos
invertStmt mode (Local assign1 body assign2 pos) =
  Local assign2 (invertStmts mode body) assign1 pos
invertStmt Locally (Call funId args pos) =
  Uncall funId args pos
invertStmt Locally (Uncall funId args pos) =
  Call funId args pos
invertStmt Locally (ExtCall funId args pos) =
  ExtUncall funId args pos
invertStmt Locally (ExtUncall funId args pos) =
  ExtCall funId args pos
invertStmt Globally stmt@(Call{}) = stmt
invertStmt Globally stmt@(Uncall{}) = stmt
invertStmt Globally stmt@(ExtCall{}) = stmt
invertStmt Globally stmt@(ExtUncall{}) = stmt
invertStmt _ stmt@(UserError{}) = stmt
invertStmt _ stmt@(Swap{}) = stmt
invertStmt _ stmt@(Prints{}) = stmt
invertStmt _ stmt@(Skip{}) = stmt
invertStmt _ stmt@(Assert{}) = stmt
invertStmt _ stmt@(Debug{}) = stmt

