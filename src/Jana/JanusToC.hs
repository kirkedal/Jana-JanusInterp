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

stmtWithBody :: Doc -> Doc -> Doc
stmtWithBody lead doc = lead <+> text "{" $+$ (nest 4 doc) $+$ text "}"

formatType (Int _)   = text "int"
-- formatType (Stack _) = text "stack"

data IdentType = Forward | Reverse | Value | Pointer | Reference

formatIdent :: IdentType -> Ident -> Doc
formatIdent Forward   id = text (ident id) <> text "_forward"
formatIdent Reverse   id = text (ident id) <> text "_reverse"
formatIdent Value     id = text (ident id)
formatIdent Reference id = text "&" <> text (ident id)
formatIdent Pointer   id = text "*" <> text (ident id)

formatLval :: Lval -> Doc
formatLval (Var id) = formatIdent Value id
formatLval (Lookup id expr) = formatIdent Value id <> cat (map (\e -> brackets $ formatExpr e) expr)

formatModOp AddEq = text "+="
formatModOp SubEq = text "-="
formatModOp XorEq = text "^="

-- Operators and their precedence
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

formatLocalDecl (LocalVar typ ident expr p)     = formatVdecl (Scalar typ ident (Just expr) p)
formatLocalDecl (LocalArray ident iexprs expr p) = formatVdecl (Array ident iexprs (Just expr) p)
formatAssertLocalDecl (LocalVar typ ident expr p) = formatStmt (Assert (BinOp EQ (LV (Var ident) p) expr) p)

formatStmts :: [Stmt] -> Doc
formatStmts = vcat . map formatStmt

formatStmt :: Stmt -> Doc
formatStmt (Assign modOp lval expr _) =
  formatLval lval <+> formatModOp modOp <+> formatExpr expr <> semi
formatStmt (If e1 s1 s2 e2 p) =
  (text "if" <+> parens (formatExpr e1)) `stmtWithBody` (formatStmts s1 $+$ formatStmt (Assert e2 p)) $+$
    elsePart
  where elsePart | null s2   = empty
                 | otherwise = (text "else") `stmtWithBody` (formatStmts s2 $+$ formatStmt (Assert (UnaryOp Not e2) p))
formatStmt (From e1 s1 s2 e2 p) =
  formatStmts ((Assert e1 p):s1) $+$
  (text "while" <+> parens (formatExpr (UnaryOp Not e2))) `stmtWithBody` 
    (formatStmts $ s2 ++ [(Assert (UnaryOp Not e1) p)] ++ s1)
-- Implement with break to avoid code duplication of s1

-- formatStmt (Push id1 id2 _) =
--   text "push" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
-- formatStmt (Pop id1 id2 _) =
--   text "pop" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
formatStmt (Local decl1 s decl2 _) =
  formatLocalDecl decl1 $+$
  formatStmts s $+$
  formatAssertLocalDecl decl2
formatStmt (Call id args _) =
  formatIdent Forward id <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt (Uncall id args _) =
  formatIdent Reverse id <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt (Swap id1 id2 p) = formatStmts [Assign XorEq id1 (LV id2 p) p, Assign XorEq id2 (LV id1 p) p, Assign XorEq id1 (LV id2 p) p]
formatStmt (UserError msg _) =
  text "printf" <> parens (text (show msg)) <> semi $+$ text "exit()" <> semi
-- formatStmt (Prints (Print str) _) =
--   text "print" <> parens (text (show str))
formatStmt (Prints (Printf str []) _) =
  text "printf" <> parens (text (show str)) <> semi
formatStmt (Prints (Printf str idents) _) =
  text "printf" <> parens (text (show str) <> comma <+> commasep (map (formatIdent Value) idents)) <> semi
-- formatStmt (Prints (Show idents) _) =
--   text "show" <> parens (commasep $ map formatIdent idents)
-- formatStmt (Skip _) =
--   text "skip"
formatStmt (Assert e _) =
  text "assert" <> parens (formatExpr e) <> semi

-- Main procedure
formatMain (ProcMain vdecls body p) =
  (formatType (Int p) <+> text "main()") `stmtWithBody`
    (vcat (map formatVdecl vdecls) $+$
      text "" $+$
      formatStmts body $+$
      text "return 1;")

formatVdecl (Scalar typ id exp _) =
  formatType typ <+> formatIdent Value id <+> formatExp exp <> semi 
  where
    formatExp (Just expr) = equals <+> formatExpr expr
    formatExp Nothing     = equals <+> integer 0
formatVdecl (Array id size a_exp p) =
  formatType (Int p) <+> formatIdent Value id <> vcat (map formatSize size) <+> formatExp a_exp <> semi
  where formatSize (Just e) = brackets $ formatExpr e
        formatSize Nothing  = brackets empty
        formatExp (Just ex) = equals $+$ formatExpr ex
        formatExp Nothing   = equals <+> braces (integer 0)

-- Local procedures
formatProc proc =
  (text "void" <+> formatIdent Forward (procname proc) <> 
    parens (formatParams $ params proc)) `stmtWithBody`
      (formatStmts $ body proc) $+$
    (text "void" <+> formatIdent Reverse (procname proc) <> 
    parens (formatParams $ params proc)) `stmtWithBody`
      (formatStmts $ invertStmts Locally $ body proc)

formatParam (Scalar typ id exp _) =
  formatType typ <+> formatIdent Reference id <> formatExp exp
    where
      formatExp (Just expr) = equals $+$ formatExpr expr
      formatExp Nothing     = empty
formatParam (Array id _size a_exp p) =
  formatType (Int p) <+> formatIdent Pointer id <> formatExp a_exp
    where
      formatExp (Just expr) = equals $+$ formatExpr expr
      formatExp Nothing     = empty

formatParams = commasep . map formatParam


-- Program
formatProgram (Program mains procs) =
  text "/* Translated from Janus program */" $+$
  text "#include <stdio.h>      /* printf */" $+$
  text "#include <assert.h>" $+$ 
  text "" $+$
  vcat (intersperse (text "") $ map formatProc procs) $+$
  text "" $+$
  vcat (intersperse (text "") $ map formatMain mains)

class ShowC a where
  showC :: a -> String

instance ShowC Type where
  showC = render . formatType

instance ShowC Ident where
  showC = render . formatIdent Value

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






