module Jana.Ast where

import Text.Parsec.Pos

-- Data types
data Type
    = Int SourcePos
    | Stack SourcePos
    | BoolT SourcePos

instance Eq Type where
  (Int _)   == (Int _)   = True
  (Stack _) == (Stack _) = True
  (BoolT _) == (BoolT _) = True
  _         == _         = False

-- Identifier
data Ident =
  Ident String SourcePos

instance Eq Ident where
  (Ident name1 _) == (Ident name2 _) = name1 == name2

-- Declaration value
data DeclVal
    = VarDecl   Expr
    | ArrayDecl [Maybe Integer] [Expr]
    deriving (Eq)

-- Left-value
data Lval
    = Var    Ident
    | Lookup Ident [Expr]
    deriving (Eq)

-- Modification operators used in assignment
data ModOp
    = AddEq -- +=
    | SubEq -- -=
    | XorEq -- ^=
    deriving (Eq, Show)

-- Unary operators
data UnaryOp
    = Not
    | FromLoop -- Hack for loops
    deriving (Eq, Ord)

-- Binary operators
data BinOp
    = Add | Sub | Mul | Div | Mod | Exp  -- Arithmetic (+ - * / % **)
    | And | Or | Xor                     -- Binary (& | ^)
    | LAnd | LOr                         -- Logical (&& ||)
    | GT | LT | EQ | NEQ | GE | LE       -- Relational (> < = != >= <=)
    deriving (Eq, Ord, Show)

data DebugType 
    = Normal
    | Beginning
    | End
    deriving (Eq)

-- Statement
data Stmt
    = Assign    ModOp Lval Expr SourcePos
    | If        Expr [Stmt] [Stmt] Expr SourcePos
    | From      Expr [Stmt] [Stmt] Expr SourcePos
    | Iterate   Type Ident Expr Expr Expr [Stmt] SourcePos
    | Push      Ident Ident SourcePos
    | Pop       Ident Ident SourcePos
    | Local     LocalDecl [Stmt] LocalDecl SourcePos
    | Call      Ident [Ident] SourcePos
    | Uncall    Ident [Ident] SourcePos
    | ExtCall   Ident [Ident] SourcePos
    | ExtUncall Ident [Ident] SourcePos
    | UserError String SourcePos
    | Swap      Lval Lval SourcePos
    | Prints    Prints SourcePos
    -- | ShowVar   Ident SourcePos
    -- | ReadVar   Ident SourcePos
    | Skip      SourcePos
    | Assert    Expr SourcePos
    | Debug     DebugType SourcePos
    deriving (Eq)

-- Local Declaration
data LocalDecl
    = LocalVar Type Ident Expr SourcePos
    | LocalArray Ident [Maybe Expr] Expr SourcePos
    deriving (Eq)

-- Expression
data Expr
    = Number   Integer SourcePos
    | Boolean  Bool SourcePos
    | LV       Lval SourcePos
    | UnaryOp  UnaryOp Expr
    | BinOp    BinOp Expr Expr
    | Empty    Ident SourcePos
    | Top      Ident SourcePos
    | Size     Ident SourcePos
    | Nil      SourcePos
    | ArrayE   [Expr] SourcePos
    deriving (Eq)

-- Declaration
data Vdecl
    = Scalar Type Ident (Maybe Expr) SourcePos 
    | Array  Ident [Maybe Expr] (Maybe Expr) SourcePos
    | StackD Ident (Maybe Expr) SourcePos
    deriving (Eq)

data Prints
    = Print String
    | Printf String [Ident]
    | Show [Ident]
    deriving (Eq)

-- Main procedure
data ProcMain
    = ProcMain [Vdecl] [Stmt] SourcePos
    deriving (Eq)

-- Procedure definition
data Proc
    = Proc { procname  :: Ident
           , params    :: [Vdecl]   -- Zero or more
           , body      :: [Stmt]
           }
    deriving (Eq)

data Program = Program (Maybe ProcMain) [Proc]


class Identifiable a where
  ident :: a -> String

instance Identifiable Ident where
  ident (Ident id _) = id

instance Identifiable Lval where
  ident (Var id) = ident id
  ident (Lookup id _) = ident id

instance Identifiable Vdecl where
  ident (Scalar _ id _ _) = ident id
  ident (Array id _ _ _) = ident id

instance Identifiable ProcMain where
  ident _ = "main"

instance Identifiable Proc where
  ident proc = ident $ procname proc


stmtPos :: Stmt -> SourcePos
stmtPos (Assign    _ _ _   p) = p
stmtPos (If        _ _ _ _ p) = p
stmtPos (From      _ _ _ _ p) = p
stmtPos (Iterate   _ _ _ _ _ _ p) = p
stmtPos (Push      _ _     p) = p
stmtPos (Pop       _ _     p) = p
stmtPos (Local     _ _ _   p) = p
stmtPos (Call      _ _     p) = p
stmtPos (Uncall    _ _     p) = p
stmtPos (ExtCall   _ _     p) = p
stmtPos (ExtUncall _ _     p) = p
stmtPos (UserError _       p) = p
stmtPos (Swap      _ _     p) = p
stmtPos (Prints    _       p) = p
stmtPos (Skip              p) = p
stmtPos (Assert    _       p) = p
stmtPos (Debug     _       p) = p

