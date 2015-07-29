module Jana.JanusToC where

import Prelude hiding (GT, LT, EQ)
import Data.List (intersperse, intercalate)
import Text.PrettyPrint
import qualified Data.Map as Map
import Jana.Ast
import Jana.Invert

-- Reversing with an extra top-level conditional
--   or at second function
-- Parsing variables with pointers

-- MISSING:
--   Order of functions

commasep = hsep . punctuate (char ',')


formatType (Int _)   = text "int"
-- formatType (Stack _) = text "stack"

data IdentType = Forward | Reverse | Value | Pointer | None

formatIdent :: IdentType -> Ident -> Doc
formatIdent Forward id = text (ident id) <> text "_forward"
formatIdent Reverse id = text (ident id) <> text "_reverse"
formatIdent Value   id = text (ident id) <> text "_val"
formatIdent Pointer id = text "*" <> text (ident id) <> text "_ptr"
formatIdent None    id = text (ident id) <> text "_ptr"

formatLval :: Lval -> Doc
formatLval (Var id) = formatIdent Pointer id
formatLval (Lookup id expr) = formatIdent None id <> brackets (vcat (intersperse (text ",") $ map (\e -> formatExpr e) expr))

formatModOp AddEq = text "+="
formatModOp SubEq = text "-="
formatModOp XorEq = text "^="

-- Operators and their precedence
-- Should match the operator table in Jana.Parser
unaryOpMap = Map.fromList [
    (Not,  ("!",  5))
  ]
binOpMap = Map.fromList [
    (Mul , ("*",  4))
  , (Div , ("/",  4))
  , (Mod , ("%",  4))

  , (Add , ("+",  3))
  , (Sub , ("-",  3))

  , (GE  , (">=", 2))
  , (GT  , (">",  2))
  , (LE  , ("<=", 2))
  , (LT  , ("<",  2))
  , (EQ  , ("==",  2))
  , (NEQ , ("!=", 2))

  , (And , ("&",  1))
  , (Or  , ("|",  1))
  , (Xor , ("^",  1))

  , (LAnd, ("&&", 0))
  , (LOr , ("||", 0))
  ]


formatUnaryOp = text . fst . (unaryOpMap Map.!)
formatBinOp = text . fst . (binOpMap Map.!)


formatExpr = f 0
  where f _ (Number num _)    = integer num
        f _ (Boolean True _)  = text "true"
        f _ (Boolean False _) = text "false"
        f _ (LV lval _)       = formatLval lval
        -- f _ (Empty id _)      = text "empty" <> parens (formatIdent id)
        -- f _ (Top id _)        = text "top" <> parens (formatIdent id)
        -- f _ (Size id _)       = text "size" <> parens (formatIdent id)
        -- f _ (ArrayE es _)     = text "{" <+> vcat (intersperse (text ",") (map formatExpr es)) $+$ text "}"
        -- f _ (Nil _)           = text "nil"
        f d (UnaryOp op e)    =
          let opd = unaryOpPrec op in
            parens' (d > opd) (formatUnaryOp op <> f opd e)
        f d (BinOp op e1 e2)  =
          let opd = binOpPrec op in
            parens' (d > opd) (f opd e1 <+> formatBinOp op <+> f opd e2)
        unaryOpPrec  = snd . (unaryOpMap Map.!)
        binOpPrec    = snd . (binOpMap Map.!)
        parens' bool = if bool then parens else id


formatVdecl (Scalar typ id exp _) =
  formatType typ <+> formatIdent Value id <+> formatExp exp <> semi $+$
    formatType typ <+> formatIdent Pointer id <+> equals <+> text "&" <> formatIdent Value id <> semi
  where
    formatExp (Just expr) = equals <+> formatExpr expr
    formatExp Nothing     = equals <+> integer 0
formatVdecl (Array id size a_exp _) =
  text "int" <+> formatIdent Value id <> vcat (map formatSize size) <+> formatExp a_exp <> semi $+$
    text "int" <+> formatIdent Pointer id <+> equals <+> text "&" <> formatIdent Value id <> text "[0]" <> semi
  where formatSize (Just e) = text "[" $+$ formatExpr e <+> text "]"
        formatSize Nothing  = text "[]"
        formatExp (Just ex) = equals $+$ formatExpr ex
        formatExp Nothing   = equals <+> text "{0}"

-- CHECK
formatLocalDecl (LocalVar typ ident expr p)     = formatVdecl (Scalar typ ident (Just expr) p)
  --formatType typ <+> formatIdent None ident <+> equals <+> formatExpr expr
-- formatLocalDecl (LocalArray ident iexprs expr p) = formatType (Int p) <+> formatIdent ident <+> vcat (map formatIndex iexprs) $+$ equals <+> 
--   formatExpr expr
--   where formatIndex (Just e) = text "[" $+$ formatExpr e <+> text "]"
--         formatIndex Nothing  = text "[]"
formatAssertLocalDecl (LocalVar typ ident expr p) = formatStmt (Assert (BinOp EQ (LV (Var ident) p) expr) p)

formatStmts :: [Stmt] -> Doc
formatStmts = vcat . map formatStmt

formatStmt :: Stmt -> Doc
formatStmt (Assign modOp lval expr _) =
  formatLval lval <+> formatModOp modOp <+> formatExpr expr <> semi
formatStmt (If e1 s1 s2 e2 p) =
  text "if (" <+> (formatExpr e1) <+> text ")" <+> text "{" $+$ 
    nest 4 (formatStmts s1 $+$
            formatStmt (Assert e2 p)) $+$
    text "}" $+$ elsePart
  where elsePart | null s2   = empty
                 | otherwise = text "else {" $+$ nest 4 (formatStmts s2 $+$ formatStmt (Assert (UnaryOp Not e2) p)) $+$ text "}"
formatStmt (From e1 s1 s2 e2 p) =
  formatStmts ((Assert e1 p):s1) $+$
  text "while" <+> parens (formatExpr (UnaryOp Not e2)) <+> text "{" $+$
    nest 4 (formatStmts $ s2 ++ [(Assert (UnaryOp Not e1) p)] ++ s1) $+$
    text "}"
-- formatStmt (Push id1 id2 _) =
--   text "push" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
-- formatStmt (Pop id1 id2 _) =
--   text "pop" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
formatStmt (Local decl1 s decl2 _) =
  formatLocalDecl decl1 $+$
  formatStmts s $+$
  formatAssertLocalDecl decl2
formatStmt (Call id args _) =
  formatIdent Forward id <> parens (commasep $ map (formatIdent None) args) <> semi
formatStmt (Uncall id args _) =
  formatIdent Reverse id <> parens (commasep $ map (formatIdent None) args) <> semi
formatStmt (Swap id1 id2 p) = formatStmts [Assign XorEq id1 (LV id2 p) p, Assign XorEq id2 (LV id1 p) p, Assign XorEq id1 (LV id2 p) p]
-- formatStmt (UserError msg _) =
--   text "error" <> parens (text (show msg))
-- formatStmt (Prints (Print str) _) =
--   text "print" <> parens (text (show str))
formatStmt (Prints (Printf str []) _) =
  text "printf" <> parens (text (show str)) <> semi
formatStmt (Prints (Printf str idents) _) =
  text "printf" <> parens (text (show str) <> comma <+> commasep (map (formatIdent Pointer) idents)) <> semi
-- formatStmt (Prints (Show idents) _) =
--   text "show" <> parens (commasep $ map formatIdent idents)
-- formatStmt (Skip _) =
--   text "skip"
formatStmt (Assert e _) =
  text "assert" <> parens (formatExpr e) <> semi


-- formatStmtsAbbrv []         = empty
-- formatStmtsAbbrv [If {}]    = text "..."
-- formatStmtsAbbrv [From {}]  = text "..."
-- formatStmtsAbbrv [Local {}] = text "..."
-- formatStmtsAbbrv [s]        = formatStmt s
-- formatStmtsAbbrv _          = text "..."


-- formatStmtAbbrv (If e1 s1 s2 e2 _) =
--   text "if" <+> formatExpr e1 <+> text "then" $+$
--     nest 4 (formatStmtsAbbrv s1) $+$
--   elsePart $+$
--   text "fi" <+> formatExpr e2
--   where elsePart | null s2   = empty
--                  | otherwise = text "else" $+$ nest 4 (formatStmtsAbbrv s2)

-- formatStmtAbbrv (From e1 s1 s2 e2 _) =
--   text "from" <+> formatExpr e1 <+> keyword $+$
--     vcat inside $+$
--   text "until" <+> formatExpr e2
--   where (keyword:inside) = doPart ++ loopPart
--         doPart   | null s1   = []
--                  | otherwise = [text "do", nest 4 (formatStmtsAbbrv s1)]
--         loopPart | null s2   = [empty]
--                  | otherwise = [text "loop", nest 4 (formatStmtsAbbrv s2)]

-- formatStmtAbbrv (Local decl1 s decl2 _) =
--   text "local" <+> formatLocalDecl decl1 $+$
--   formatStmtsAbbrv s $+$
--   text "delocal" <+> formatLocalDecl decl2

-- formatStmtAbbrv s = formatStmt s


formatMain (ProcMain vdecls body _) =
  text "int main() {" $+$
    nest 4 (vcat (map formatVdecl vdecls) $+$
            text "" $+$
            formatStmts body $+$
            text "return 1;") $+$
    text "}"


formatParam (Scalar typ id exp _) =
  formatType typ <+> formatIdent Pointer id <> formatExp exp
    where
      formatExp (Just expr) = equals $+$ formatExpr expr
      formatExp Nothing     = empty
formatParam (Array id _size a_exp p) =
  formatType (Int p) <+> formatIdent Pointer id <> formatExp a_exp
    where
      formatExp (Just expr) = equals $+$ formatExpr expr
      formatExp Nothing     = empty

-- formatParam (Array id size a_exp _) =
--   text "int" <+> formatIdent id <> vcat (map formatSize size) $+$ formatExp a_exp
--   where formatSize (Just e) = text "[" $+$ formatExpr e <+> text "]"
--         formatSize Nothing  = text "[]"
--         formatExp (Just ex) = equals $+$ formatExpr ex
--         formatExp Nothing   = empty

formatParams = commasep . map formatParam

formatProc proc =
  text "void" <+> formatIdent Forward (procname proc) <> 
    parens (formatParams $ params proc) <+> text "{" $+$
    nest 4 (formatStmts $ body proc) $+$
    text "}" $+$
    text "void" <+> formatIdent Reverse (procname proc) <> 
    parens (formatParams $ params proc) <+> text "{" $+$
    nest 4 (formatStmts $ invertStmts Locally $ body proc) $+$
    text "}"



formatProgram (Program [main] procs) =
  text "/* Translated from Janus program */" $+$
  text "#include <stdio.h>      /* printf */" $+$
  text "#include <assert.h>" $+$ 
  text "" $+$
  vcat (intersperse (text "") $ map formatProc procs) $+$
  text "" $+$
  formatMain main

class ShowC a where
  showC :: a -> String

instance ShowC Type where
  showC = render . formatType

instance ShowC Ident where
  showC = render . formatIdent None

instance ShowC Lval where
  showC = render . formatLval

instance ShowC Expr where
  showC = render . formatExpr

instance ShowC Stmt where
  showC = render . formatStmt

instance ShowC Vdecl where
  showC = render . formatVdecl

instance ShowC Proc where
  showC = render . formatProc

instance ShowC ProcMain where
  showC = render . formatMain

instance ShowC Program where
  showC = render . formatProgram






