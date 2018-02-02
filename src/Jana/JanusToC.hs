module Jana.JanusToC where

import Prelude hiding (GT, LT, EQ)
import Data.List (intersperse)
import Text.PrettyPrint
import qualified Data.Map as Map
import Jana.Ast
import Jana.Invert

-- Reversing with an extra top-level conditional
--   or at second function
-- Parsing variables with pointers

-- MISSING:
--   Define function instances
--   Add header file

commasep :: [Doc] -> Doc
commasep = hsep . punctuate (char ',')

stmtWithBody :: Doc -> Doc -> Doc
stmtWithBody lead doc = lead <+> text "{" $+$ (nest 4 doc) $+$ text "}"

formatType :: Type -> Doc
formatType (Int _)   = text "int"
formatType (Stack _) = error "Stack not supported in C Translation"
formatType (BoolT _) = error "Stack not supported in C Translation"

data IdentType = Forward | Reverse | Value | Pointer Int | Reference

formatIdent :: IdentType -> Ident -> Doc
formatIdent Forward     idnt = text (ident idnt) <> text "_forward"
formatIdent Reverse     idnt = text (ident idnt) <> text "_reverse"
formatIdent Value       idnt = text (ident idnt)
formatIdent Reference   idnt = text "&" <> text (ident idnt)
formatIdent (Pointer n) idnt = text (replicate n '*') <> text (ident idnt)

formatLval :: Lval -> Doc
formatLval (Var idnt) = formatIdent Value idnt
formatLval (Lookup idnt expr) = formatIdent Value idnt <> cat (map (\e -> brackets $ formatExpr e) expr)

formatModOp :: ModOp -> Doc
formatModOp AddEq = text "+="
formatModOp SubEq = text "-="
formatModOp XorEq = text "^="

-- Operators and their precedence
unaryOpMap :: Map.Map UnaryOp ([Char], Integer)
unaryOpMap = Map.fromList [
    (Not,  ("!",  5))
  ]
binOpMap :: Map.Map BinOp ([Char], Integer)
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


formatUnaryOp :: UnaryOp -> Doc
formatUnaryOp = text . fst . (unaryOpMap Map.!)
formatBinOp :: BinOp -> Doc
formatBinOp = text . fst . (binOpMap Map.!)

formatExpr :: Expr -> Doc
formatExpr = f 0
  where f _ (Number num _)    = integer num
        f _ (Boolean True _)  = text "true"
        f _ (Boolean False _) = text "false"
        f _ (LV lval _)       = formatLval lval
        f _ (Empty _ _)      = error "Not supported in C++ traslation"
        f _ (Top _ _)        = error "Not supported in C++ traslation"
        f _ (Size _ _)       = error "Not supported in C++ traslation"
        f _ (ArrayE _ _)     = error "Not supported in C++ traslation"
        f _ (Nil _)           = error "Not supported in C++ traslation"
        f d (UnaryOp op e)    =
          let opd = unaryOpPrec op in
            parens' (d > opd) (formatUnaryOp op <> f opd e)
        f d (BinOp op e1 e2)  =
          let opd = binOpPrec op in
            parens' (d > opd) (f opd e1 <+> formatBinOp op <+> f opd e2)
        unaryOpPrec  = snd . (unaryOpMap Map.!)
        binOpPrec    = snd . (binOpMap Map.!)
        parens' bool = if bool then parens else id

formatLocalDecl :: LocalDecl -> Doc
formatLocalDecl (LocalVar typ idnt expr p)     = formatVdecl (Scalar typ idnt expr p)
formatLocalDecl (LocalArray idnt iexprs expr p) = formatVdecl (Array idnt iexprs expr p)

formatAssertLocalDecl :: LocalDecl -> Doc
formatAssertLocalDecl (LocalVar tp idnt Nothing p) = formatStmt (Assert (BinOp EQ (LV (Var idnt) p) (baseVal tp)) p)
formatAssertLocalDecl (LocalVar _ idnt (Just expr) p) = formatStmt (Assert (BinOp EQ (LV (Var idnt) p) expr) p)
formatAssertLocalDecl (LocalArray _ _ _ _) = error "Not implemented"

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

formatStmt (Iterate typ idnt startE stepE endE stmts _) =
  (text "for" <+>
    parens (formatType typ <+> formatIdent Value idnt <+> text "=" <+> formatExpr startE <+>
      text ";" <+> formatIdent Value idnt <+> text "!=" <+> formatExpr (BinOp Add endE stepE) <+>
      text ";" <+> formatIdent Value idnt <+> text "+=" <+> formatExpr stepE)) `stmtWithBody`
    formatStmts stmts

formatStmt (Push _ _ _) = error "Translation of push to C is not supported"
formatStmt (Pop _ _ _) = error "Translation of push to C is not supported"
formatStmt (Local decl1 s decl2 _) =
  formatLocalDecl decl1 $+$
  formatStmts s $+$
  formatAssertLocalDecl decl2
formatStmt (Call idnt args _) =
  formatIdent Forward idnt <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt (Uncall idnt args _) =
  formatIdent Reverse idnt <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt (ExtCall idnt args _) =
  formatIdent Forward idnt <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt (ExtUncall idnt args _) =
  formatIdent Reverse idnt <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt (Swap id1 id2 p) =
  formatStmts [Assign XorEq id1 (LV id2 p) p, Assign XorEq id2 (LV id1 p) p, Assign XorEq id1 (LV id2 p) p]
formatStmt (UserError msg _) =
  text "printf" <> parens (text (show msg)) <> semi $+$ text "exit()" <> semi
formatStmt (Prints (Print _) _) = error "Not supported in C++ traslation"
formatStmt (Prints (Printf str []) _) =
  text "printf" <> parens (text (show str)) <> semi
formatStmt (Prints (Printf str idents) _) =
  text "printf" <> parens (text (show str) <> comma <+> commasep (map (formatIdent Value) idents)) <> semi
formatStmt (Prints (Show _) _) = error "Not supported in C++ traslation"
formatStmt (Skip _) = empty
formatStmt (Assert e _) =
  text "assert" <> parens (formatExpr e) <> semi
formatStmt (Debug _ _) = error "Not supported in C++ traslation"

-- Main procedure
formatMain :: ProcMain -> Doc
formatMain (ProcMain vdecls mainbody p) =
  (formatType (Int p) <+> text "main()") `stmtWithBody`
    (vcat (map formatVdecl vdecls) $+$
      text "" $+$
      formatStmts mainbody $+$
      text "return 1;")

formatVdecl :: Vdecl -> Doc
formatVdecl (Scalar typ idnt expr _) =
  formatType typ <+> formatIdent Value idnt <+> formatExp expr <> semi
  where
    formatExp (Just e) = equals <+> formatExpr e
    formatExp Nothing  = equals <+> integer 0
formatVdecl (Array idnt size a_exp p) =
  formatType (Int p) <+> formatIdent Value idnt <> vcat (map formatSize size) <+> formatExp a_exp <> semi
  where formatSize (Just e) = brackets $ formatExpr e
        formatSize Nothing  = brackets empty
        formatExp (Just ex) = equals $+$ formatExpr ex
        formatExp Nothing   = equals <+> braces (integer 0)

-- Local procedures
formatProc :: Proc -> Doc
formatProc proc =
  (text "void" <+> formatIdent Forward (procname proc) <>
    parens (formatParams $ params proc)) `stmtWithBody`
      (formatStmts $ body proc) $+$
    (text "void" <+> formatIdent Reverse (procname proc) <>
    parens (formatParams $ params proc)) `stmtWithBody`
      (formatStmts $ invertStmts Locally $ body proc)

formatParam :: Vdecl -> Doc
formatParam (Scalar typ idnt expr _) =
  formatType typ <+> formatIdent Reference idnt <> formatExp expr
    where
      formatExp (Just e) = equals $+$ formatExpr e
      formatExp Nothing  = empty
formatParam (Array idnt size a_exp p) =
  formatType (Int p) <+> formatIdent (Pointer (length size)) idnt <> formatExp a_exp
    where
      formatExp (Just expr) = equals $+$ formatExpr expr
      formatExp Nothing     = empty

formatParams :: [Vdecl] -> Doc
formatParams = commasep . map formatParam

defineProc :: Proc -> Doc
defineProc proc =
  (text "void" <+> formatIdent Forward (procname proc) <>
    parens (formatParams $ params proc)) <> text ";"
      $+$
    (text "void" <+> formatIdent Reverse (procname proc) <>
    parens (formatParams $ params proc)) <> text ";"


-- Program
formatProgram :: Maybe [Char] -> Program -> Doc
formatProgram headerfile (Program mains procs) =
  text "/* Translated from Janus program */" $+$
  text "#include <stdio.h>      /* printf */" $+$
  text "#include <assert.h>" $+$
  text include_header $+$
  text "" $+$
  vcat (intersperse (text "") $ map defineProc procs) $+$
  text "" $+$
  vcat (intersperse (text "") $ map formatProc procs) $+$
  text "" $+$
  maybe empty formatMain mains
  where
    include_header =
      case headerfile of
        Nothing   -> ""
        (Just file) -> "#include \"" ++ file ++ "\""

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
  showC = render . formatProgram Nothing






