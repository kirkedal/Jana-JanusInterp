
module Jana.Format where

import Prelude hiding (GT, LT, EQ)
import Data.List (intersperse)
import Text.PrettyPrint
import qualified Data.Map as Map
import Jana.Ast


commasep :: [Doc] -> Doc
commasep = hsep . punctuate (char ',')


formatType :: Type -> Doc
formatType (Int it _) = formatIntType it
formatType (Stack _)  = text "stack"
formatType (BoolT _)  = text "bool"

formatIntType :: IntType -> Doc
formatIntType Unbound = text "int"
formatIntType I8      = text "i8"
formatIntType I16     = text "i16"
formatIntType I32     = text "i32"
formatIntType I64     = text "i64"
formatIntType U8      = text "u8"
formatIntType U16     = text "u16"
formatIntType U32     = text "u32"
formatIntType U64     = text "u84"


formatIdent :: Ident -> Doc
formatIdent fid = text (ident fid)

formatLval :: Lval -> Doc
formatLval (Var fid) = formatIdent fid
formatLval (Lookup fid expr) = formatIdent fid <> brackets (hcat (intersperse (text ",") $ map (\e -> formatExpr e) expr))

formatModOp :: ModOp -> Doc
formatModOp AddEq = text "+="
formatModOp SubEq = text "-="
formatModOp XorEq = text "^="

-- Operators and their precedence
-- Should match the operator table in Jana.Parser
unaryOpMap :: Map.Map UnaryOp ([Char], Integer)
unaryOpMap = Map.fromList [
    (Not,  ("!",  5))
  , (BwNeg,("~",  5))
  ]

binOpMap :: Map.Map BinOp ([Char], Integer)
binOpMap = Map.fromList [
    (Mul , ("*",  4))
  , (Div , ("/",  4))
  , (Mod , ("%",  4))
  , (Exp , ("**", 4))

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


formatUnaryOp :: UnaryOp -> Doc
formatUnaryOp = text . fst . (unaryOpMap Map.!)

formatBinOp :: BinOp -> Doc
formatBinOp = text . fst . (binOpMap Map.!)


formatExpr :: Expr -> Doc
formatExpr = f 0
  where
    f :: Integer -> Expr -> Doc
    f _ (Number num _)      = integer num
    f _ (Boolean True _)    = text "true"
    f _ (Boolean False _)   = text "false"
    f _ (LV lval _)         = formatLval lval
    f _ (Empty fid _)       = text "empty" <> parens (formatIdent fid)
    f _ (Top fid _)         = text "top" <> parens (formatIdent fid)
    f _ (Size fid _)        = text "size" <> parens (formatIdent fid)
    f _ (ArrayE es _)       = text "{" <+> vcat (intersperse (text ",") (map formatExpr es)) $+$ text "}"
    f _ (Nil _)             = text "nil"
    f d (TypeCast typ expr) = parens' (d > 6) (formatType typ <+> f 6 expr)
    f d (UnaryOp op e)      =
      let opd = unaryOpPrec op in
        parens' (d > opd) (formatUnaryOp op <> f opd e)
    f d (BinOp op e1 e2)  =
      let opd = binOpPrec op in
        parens' (d > opd) (f opd e1 <+> formatBinOp op <+> f opd e2)
    unaryOpPrec  = snd . (unaryOpMap Map.!)
    binOpPrec    = snd . (binOpMap Map.!)
    parens' bool = if bool then parens else id

formatMaybeExpr :: Maybe Expr -> Doc
formatMaybeExpr Nothing  = empty
formatMaybeExpr (Just e) = equals <+> formatExpr e


formatVdecl :: Vdecl -> Doc
formatVdecl (Scalar typ fid expr _) =
  formatType typ <+> formatIdent fid $+$ formatExp expr
    where
      formatExp (Just e) = equals $+$ formatExpr e
      formatExp Nothing     = empty
formatVdecl (Array itype fid size a_exp _) =
  formatIntType itype <+> formatIdent fid <> vcat (map formatSize size) $+$ formatExp a_exp
  where formatSize (Just e) = text "[" $+$ formatExpr e <+> text "]"
        formatSize Nothing  = text "[]"
        formatExp (Just ex) = equals $+$ formatExpr ex
        formatExp Nothing   = empty


formatLocalDecl :: LocalDecl -> Doc
formatLocalDecl (LocalVar typ varId expr _)     = formatType typ <+> formatIdent varId <+> formatMaybeExpr expr
formatLocalDecl (LocalArray it arrId iexprs expr p) = formatType (Int it p) <+> formatIdent arrId <+> vcat (map formatIndex iexprs) $+$ formatMaybeExpr expr
  where formatIndex (Just e) = text "[" $+$ formatExpr e <+> text "]"
        formatIndex Nothing  = text "[]"

formatStmts :: [Stmt] -> Doc
formatStmts = vcat . map formatStmt

formatStmt :: Stmt -> Doc
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
formatStmt (Iterate  typ iterId startE stepE endE stmts _) =
  text "iterate" <+> formatType typ <+> formatIdent iterId <+> text "=" <+> formatExpr startE <+>
    text "by" <+> formatExpr stepE <+> text "to" <+> formatExpr endE $+$
    nest 4 (formatStmts stmts) $+$
    text "end"
formatStmt (Push id1 id2 _) =
  text "push" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
formatStmt (Pop id1 id2 _) =
  text "pop" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
formatStmt (Local decl1 s decl2 _) =
  text "local" <+> formatLocalDecl decl1 $+$
  formatStmts s $+$
  text "delocal" <+> formatLocalDecl decl2
formatStmt (Call fid args _) =
  text "call" <+> formatIdent fid <> parens (commasep $ map formatArgument args)
formatStmt (Uncall fid args _) =
  text "uncall" <+> formatIdent fid <> parens (commasep $ map formatArgument args)
formatStmt (ExtCall fid args _) =
  text "call external" <+> formatIdent fid <> parens (commasep $ map formatArgument args)
formatStmt (ExtUncall fid args _) =
  text "uncall external" <+> formatIdent fid <> parens (commasep $ map formatArgument args)
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
formatStmt (Assert e _) =
  text "assert" <> parens (formatExpr e)
formatStmt (Debug _ _) =
  text ""

formatArgument :: Argument -> Doc
formatArgument (VarArg i) = formatIdent i
formatArgument (ArrayArg a i) = formatIdent a <> (brackets $ commasep (map formatIdent i))


formatStmtsAbbrv :: [Stmt] -> Doc
formatStmtsAbbrv []         = empty
formatStmtsAbbrv [If {}]    = text "..."
formatStmtsAbbrv [From {}]  = text "..."
formatStmtsAbbrv [Local {}] = text "..."
formatStmtsAbbrv [s]        = formatStmt s
formatStmtsAbbrv _          = text "..."

formatStmtAbbrv :: Stmt -> Doc
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


formatMain :: ProcMain -> Doc
formatMain (ProcMain vdecls mbody _) =
  text "procedure main()" $+$
    nest 4 (vcat (map formatVdecl vdecls) $+$
            text "" $+$
            formatStmts mbody)

formatParams :: [Vdecl] -> Doc
formatParams = commasep . map formatVdecl

formatProc :: Proc -> Doc
formatProc proc =
  text "procedure" <+> formatIdent (procname proc) <>
  parens (formatParams $ params proc) $+$
    nest 4 (formatStmts $ body proc)

formatProgram :: Program -> Doc
formatProgram (Program main procs) =
  vcat (intersperse (text "") $ map formatProc procs) $+$
  text "" $+$
  maybe empty formatMain main

-- Show instance for AST
instance Show Type where
  show = render . formatType

instance Show IntType where
  show = render . formatIntType

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

instance Show Argument where
  show = render . formatArgument

instance Show ProcMain where
  show = render . formatMain

instance Show Program where
  show = render . formatProgram
