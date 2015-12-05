module RevTM.JanusToTMC where

import Prelude hiding (GT, LT, EQ)
import Data.List (intersperse, intercalate)
import Text.PrettyPrint
import qualified Data.Map as Map
import Jana.Ast
import Jana.Invert

import Control.Monad.Reader
import qualified Data.Map as Map

import RevTM.Types

-------------------------------------------------------------------------------
-- ** State monad for list of shared variables.
-------------------------------------------------------------------------------

type SharedVars = [TMVar]

sharedIdent :: Ident -> SharedVars -> Bool
sharedIdent (Ident id _) sharedVars = elem id sharedVars

-- isSharedVar s =
--   do 
--     sv <- ask
--     return $ elem s sv
isSharedVar :: SharedVars -> Lval -> Bool
isSharedVar sv (Var id) = sharedIdent id sv
isSharedVar sv (Lookup id _) = sharedIdent id sv 

addSharedRead :: SharedVars -> Lval -> Doc
addSharedRead sv lval 
  | isSharedVar sv lval = text "TM_SHARED_READ_F" <> parens (formatLval sv lval)
  | otherwise           = formatLval sv lval

addSharedWrite :: SharedVars -> Lval -> Expr -> Doc
addSharedWrite sv lval expr 
  | isSharedVar sv lval = text "TM_SHARED_WRITE_F" <> parens (formatLval sv lval <> text "," <> formatExpr sv expr)
  | otherwise           = formatLval sv lval <+> text "=" <+> formatExpr sv expr

-------------------------------------------------------------------------------
-- ** 
-------------------------------------------------------------------------------

formatRevTM :: SharedVars -> [Stmt] -> Doc
formatRevTM sharedVars stmts =
  text "TM_BEGIN();" $+$
  formatStmts sharedVars stmts $+$
  text "TM_END();" $+$
  text ""


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

formatIdent :: IdentType -> Ident ->  Doc
formatIdent Forward   id = text (ident id) <> text "_forward"
formatIdent Reverse   id = text (ident id) <> text "_reverse"
formatIdent Value     id = text (ident id)
formatIdent Reference id = text "&" <> text (ident id)
formatIdent Pointer   id = text "*" <> text (ident id)

formatLval :: SharedVars -> Lval ->  Doc
formatLval _  (Var id) = formatIdent Value id
formatLval sv (Lookup id expr) = formatIdent Value id <> brackets (vcat (intersperse (text ",") $ map (\e -> formatExpr sv e) expr))


transformModOp AddEq = Add
transformModOp SubEq = Sub
transformModOp XorEq = Xor

formatModOp AddEq = text "+"
formatModOp SubEq = text "-"
formatModOp XorEq = text "^"

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
  , (EQ  , ("==", 2))
  , (NEQ , ("!=", 2))

  , (And , ("&",  1))
  , (Or  , ("|",  1))
  , (Xor , ("^",  1))

  , (LAnd, ("&&", 0))
  , (LOr , ("||", 0))
  ]


formatUnaryOp = text . fst . (unaryOpMap Map.!)
formatBinOp = text . fst . (binOpMap Map.!)

formatExpr :: SharedVars ->  Expr -> Doc
formatExpr sv = f 0
  where f _ (Number num _)    = integer num
        f _ (Boolean True _)  = text "true"
        f _ (Boolean False _) = text "false"
        f _ (LV lval _)       = addSharedRead sv lval
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


-- MALLOC
-- THIS MUST BE UPDATED WITH MALLOC? 
formatLocalDecl sv (LocalVar typ ident expr p)       = formatVdecl sv (Scalar typ ident (Just expr) p)
formatLocalDecl sv (LocalArray ident iexprs expr p)  = formatVdecl sv (Array ident iexprs (Just expr) p)
formatAssertLocalDecl sv (LocalVar typ ident expr p) = formatStmt  sv (Assert (BinOp EQ (LV (Var ident) p) expr) p)

formatStmts :: SharedVars -> [Stmt] -> Doc
formatStmts sv = vcat . map (formatStmt sv)

formatStmt :: SharedVars -> Stmt -> Doc
formatStmt sv (Assign modOp lval expr p) =
  (addSharedWrite sv lval (BinOp (transformModOp modOp) (LV lval p) expr)) <> semi
formatStmt sv (If e1 s1 s2 e2 p) =
  (text "if" <+> parens (formatExpr sv e1)) `stmtWithBody` (formatStmts sv s1 $+$ formatStmt sv (Assert e2 p)) $+$
    elsePart
  where elsePart | null s2   = empty
                 | otherwise = (text "else") `stmtWithBody` (formatStmts sv s2 $+$ formatStmt sv (Assert (UnaryOp Not e2) p))
formatStmt sv (From e1 s1 s2 e2 p) =
  formatStmts sv ((Assert e1 p):s1) $+$
  (text "while" <+> parens (formatExpr sv (UnaryOp Not e2))) `stmtWithBody` 
    (formatStmts sv $ s2 ++ [(Assert (UnaryOp Not e1) p)] ++ s1)
-- Implement with break to avoid code duplication of s1

-- formatStmt (Push id1 id2 _) =
--   text "push" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
-- formatStmt (Pop id1 id2 _) =
--   text "pop" <> parens (formatIdent id1 <> comma <+> formatIdent id2)
formatStmt sv (Local decl1 s decl2 _) =
  formatLocalDecl sv decl1 $+$
  formatStmts sv s $+$
  formatAssertLocalDecl sv decl2
formatStmt sv (Call id args _) =
  formatIdent Forward id <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt sv (Uncall id args _) =
  formatIdent Reverse id <> parens (commasep $ map (formatIdent Value) args) <> semi
formatStmt sv (Swap id1 id2 p) = formatStmts sv [Assign XorEq id1 (LV id2 p) p, Assign XorEq id2 (LV id1 p) p, Assign XorEq id1 (LV id2 p) p]
-- formatStmt (UserError msg _) =
--   text "error" <> parens (text (show msg))
-- formatStmt (Prints (Print str) _) =
--   text "print" <> parens (text (show str))
formatStmt sv (Prints (Printf str []) _) =
  text "printf" <> parens (text (show str)) <> semi
formatStmt sv (Prints (Printf str idents) _) =
  text "printf" <> parens (text (show str) <> comma <+> commasep (map (formatIdent Value) idents)) <> semi
-- formatStmt (Prints (Show idents) _) =
--   text "show" <> parens (commasep $ map formatIdent idents)
-- formatStmt (Skip _) =
--   text "skip"
formatStmt sv (Assert e _) =
  text "assert" <> parens (formatExpr sv e) <> semi

-- Main procedure
-- formatMain (ProcMain vdecls body p) =
--   (formatType (Int p) <+> text "main()") `stmtWithBody`
--     (vcat (map formatVdecl vdecls) $+$
--       text "" $+$
--       formatStmts body $+$
--       text "return 1;")

formatVdecl sv (Scalar typ id exp _) =
  formatType typ <+> formatIdent Value id <+> formatExp exp <> semi 
  where
    formatExp (Just expr) = equals <+> formatExpr sv expr
    formatExp Nothing     = equals <+> integer 0
formatVdecl sv (Array id size a_exp p) =
  formatType (Int p) <+> formatIdent Value id <> vcat (map formatSize size) <+> formatExp a_exp <> semi
  where formatSize (Just e) = brackets $ formatExpr sv e
        formatSize Nothing  = brackets empty
        formatExp (Just ex) = equals $+$ formatExpr sv ex
        formatExp Nothing   = equals <+> braces (integer 0)

-- Local procedures
-- formatProc proc =
--   (text "void" <+> formatIdent Forward (procname proc) <> 
--     parens (formatParams $ params proc)) `stmtWithBody`
--       (formatStmts $ body proc) $+$
--     (text "void" <+> formatIdent Reverse (procname proc) <> 
--     parens (formatParams $ params proc)) `stmtWithBody`
--       (formatStmts $ invertStmts Locally $ body proc)

-- formatParam (Scalar typ id exp _) =
--   formatType typ <+> formatIdent Reference id <> formatExp exp
--     where
--       formatExp (Just expr) = equals $+$ formatExpr expr
--       formatExp Nothing     = empty
-- formatParam (Array id _size a_exp p) =
--   formatType (Int p) <+> formatIdent Pointer id <> formatExp a_exp
--     where
--       formatExp (Just expr) = equals $+$ formatExpr expr
--       formatExp Nothing     = empty

-- formatParams = commasep . map formatParam


-- Program
-- formatProgram (Program mains procs) =
--   text "/* Translated from Janus program */" $+$
--   text "#include <stdio.h>      /* printf */" $+$
--   text "#include <assert.h>" $+$ 
--   text "" $+$
--   vcat (intersperse (text "") $ map formatProc procs) $+$
--   text "" $+$
--   vcat (intersperse (text "") $ map formatMain mains)



-- class ShowTMC a where
--   showTMC :: a -> String

-- instance ShowTMC Type where
--   showTMC = render . formatType

-- instance ShowTMC Ident where
--   showTMC = render . formatIdent Value

-- instance ShowTMC Lval where
--   showTMC = render . formatLval

-- instance ShowTMC Expr where
--   showTMC = render . formatExpr

-- instance ShowTMC Stmt where
--   showTMC = render . formatStmt

-- instance ShowTMC Vdecl where
--   showTMC = render . formatVdecl




-- instance ShowTMC Proc where
--   showC = render . formatProc

-- instance ShowTMC ProcMain where
--   showC = render . formatMain

-- instance ShowTMC Program where
--   showC = render . formatProgram






