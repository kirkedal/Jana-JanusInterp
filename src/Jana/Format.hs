
module Jana.Format where

import Prelude hiding (GT, LT, EQ)
import Data.List (intersperse, intercalate)
import Text.PrettyPrint
import qualified Data.Map as Map
import Jana.Ast


commasep = hsep . punctuate (char ',')


formatType (Int _)   = text "int"
formatType (Stack _) = text "stack"

formatIdent :: Ident -> Doc
formatIdent id = text (ident id)

formatLval :: Lval -> Doc
formatLval (Var id) = formatIdent id
formatLval (Lookup id expr) = formatIdent id <> brackets (formatExpr expr)

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
  , (EQ  , ("=",  2))
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
        f _ (Empty id _)      = text "empty" <> parens (formatIdent id)
        f _ (Top id _)        = text "top" <> parens (formatIdent id)
        f _ (Size id _)       = text "size" <> parens (formatIdent id)
        f _ (Nil _)           = text "nil"
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
  formatType typ <+> formatIdent id $+$ formatExp exp
    where
      formatExp (Just expr) = equals $+$ formatExpr expr
      formatExp Nothing     = empty

formatVdecl (Array id size exps _) =
  text "int" <+> formatIdent id <> brackets (formatSize size) $+$ formatExps exps
  where formatSize (Just x)     = formatExpr x
        formatSize Nothing      = empty
        formatExps (Just exprs) = equals $+$ text "{" <+> vcat (intersperse (text ",") (map formatExpr exprs)) $+$ text "}"
        formatExps Nothing      = empty


formatLocalDecl (LocalVar typ ident expr _)     = formatType typ <+> formatIdent ident <+> equals <+> formatExpr expr
formatLocalDecl (LocalArray ident (Just expr) exprs p) = formatType (Int p) <+> formatIdent ident <+> text "[" $+$ formatExpr expr <+> text "]" $+$ equals <+> 
  text "{" <+> vcat (intersperse (text ",") (map formatExpr exprs)) $+$ text "}"
formatLocalDecl (LocalArray ident Nothing exprs p) = formatType (Int p) <+> formatIdent ident <+> text "[]" $+$ equals <+> 
  text "{" <+> vcat (intersperse (text ",") (map formatExpr exprs)) $+$ text "}"


formatStmts = vcat . map formatStmt


formatStmt (Assign modOp lval expr _) =
  formatLval lval <+> formatModOp modOp <+> formatExpr expr

formatStmt (If e1 s1 s2 e2 _) =
  text "if" <+> formatExpr e1 <+> text "then" $+$
    nest 4 (formatStmts s1) $+$
  elsePart $+$
  text "fi" <+> formatExpr e2
  where elsePart | null s2   = empty
                 | otherwise = text "else" $+$ nest 4 (formatStmts s2)

formatStmt (From e1 s1 s2 e2 _) =
  text "from" <+> formatExpr e1 <+> keyword $+$
    vcat inside $+$
  text "until" <+> formatExpr e2
  where (keyword:inside) = doPart ++ loopPart
        doPart   | null s1   = []
                 | otherwise = [text "do", nest 4 (formatStmts s1)]
        loopPart | null s2   = [empty]
                 | otherwise = [text "loop", nest 4 (formatStmts s2)]

formatStmt (Push id1 id2 _) =
  text "push" <> parens (formatIdent id1 <> comma <+> formatIdent id2)

formatStmt (Pop id1 id2 _) =
  text "pop" <> parens (formatIdent id1 <> comma <+> formatIdent id2)

formatStmt (Local decl1 s decl2 _) =
  text "local" <+> formatLocalDecl decl1 $+$
  formatStmts s $+$
  text "delocal" <+> formatLocalDecl decl2

formatStmt (Call id args _) =
  text "call" <+> formatIdent id <> parens (commasep $ map formatIdent args)

formatStmt (Uncall id args _) =
  text "uncall" <+> formatIdent id <> parens (commasep $ map formatIdent args)

formatStmt (Swap id1 id2 _) =
  formatLval id1 <+> text "<=>" <+> formatLval id2

formatStmt (UserError msg _) =
  text "error" <> parens (text (show msg))

formatStmt (Prints (Print str) _) =
  text "print" <> parens (text (show str))

formatStmt (Prints (Printf str []) _) =
  text "printf" <> parens (text (show str))

formatStmt (Prints (Printf str idents) _) =
  text "printf" <> parens (text (show str) <> comma <+> commasep (map formatIdent idents))

formatStmt (Prints (Show idents) _) =
  text "show" <> parens (commasep $ map formatIdent idents)

formatStmt (Skip _) =
  text "skip"


formatStmtsAbbrv []         = empty
formatStmtsAbbrv [If {}]    = text "..."
formatStmtsAbbrv [From {}]  = text "..."
formatStmtsAbbrv [Local {}] = text "..."
formatStmtsAbbrv [s]        = formatStmt s
formatStmtsAbbrv _          = text "..."


formatStmtAbbrv (If e1 s1 s2 e2 _) =
  text "if" <+> formatExpr e1 <+> text "then" $+$
    nest 4 (formatStmtsAbbrv s1) $+$
  elsePart $+$
  text "fi" <+> formatExpr e2
  where elsePart | null s2   = empty
                 | otherwise = text "else" $+$ nest 4 (formatStmtsAbbrv s2)

formatStmtAbbrv (From e1 s1 s2 e2 _) =
  text "from" <+> formatExpr e1 <+> keyword $+$
    vcat inside $+$
  text "until" <+> formatExpr e2
  where (keyword:inside) = doPart ++ loopPart
        doPart   | null s1   = []
                 | otherwise = [text "do", nest 4 (formatStmtsAbbrv s1)]
        loopPart | null s2   = [empty]
                 | otherwise = [text "loop", nest 4 (formatStmtsAbbrv s2)]

formatStmtAbbrv (Local decl1 s decl2 _) =
  text "local" <+> formatLocalDecl decl1 $+$
  formatStmtsAbbrv s $+$
  text "delocal" <+> formatLocalDecl decl2

formatStmtAbbrv s = formatStmt s


formatMain (ProcMain vdecls body _) =
  text "procedure main()" $+$
    nest 4 (vcat (map formatVdecl vdecls) $+$
            text "" $+$
            formatStmts body)


formatParams = commasep . map formatVdecl

formatProc proc =
  text "procedure" <+> formatIdent (procname proc) <>
  parens (formatParams $ params proc) $+$
    nest 4 (formatStmts $ body proc)


formatProgram (Program [main] procs) =
  vcat (intersperse (text "") $ map formatProc procs) $+$
  text "" $+$
  formatMain main



instance Show Type where
  show = render . formatType

instance Show Ident where
  show = render . formatIdent

instance Show Lval where
  show = render . formatLval

instance Show Expr where
  show = render . formatExpr

instance Show Stmt where
  show = render . formatStmt

instance Show Vdecl where
  show = render . formatVdecl

instance Show Proc where
  show = render . formatProc

instance Show ProcMain where
  show = render . formatMain

instance Show Program where
  show = render . formatProgram
