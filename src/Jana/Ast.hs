module Jana.Ast where

import Text.Parsec.Pos

-- Data types
data Type
    = Int IntType SourcePos
    | Stack SourcePos
    | BoolT SourcePos

data IntType
    = FreshVar
    | Unbound
    | I8
    | I16
    | I32
    | I64
    | U8
    | U16
    | U32
    | U64
    | InferInt
    deriving (Eq)


instance Eq Type where
  (Int it1 _) == (Int it2 _) = it1 == it2
  (Stack _)   == (Stack _)   = True
  (BoolT _)   == (BoolT _)   = True
  _           == _           = False

baseVal :: Type -> Expr
baseVal (Int _ sp) = Number 0 sp
baseVal (BoolT sp) = Boolean False sp
baseVal (Stack sp) = Empty (Ident "" sp) sp

typeOfIntType :: Type -> IntType
typeOfIntType (Int itype _) = itype
typeOfIntType (Stack _)     = Unbound
typeOfIntType (BoolT _)     = Unbound


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
    | MulEq -- *=
    | DivEq -- /=
    | ExpEq -- **=
    | RtEq  -- */=, √
    deriving (Eq, Show)

-- Unary operators
data UnaryOp
    = Not
    | BwNeg
    | FromLoop -- Hack for loops
    deriving (Eq, Ord)

-- Binary operators
data BinOp
    = Add | Sub | Mul | Div | Mod | Exp | Rt -- Arithmetic (+ - * / % ** */)
    | And | Or | Xor                     -- Binary (& | ^)
    | SL | SR                            -- Shift (<< | >>)
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
    | Call      Ident [Expr] SourcePos
    | Uncall    Ident [Expr] SourcePos
    | ExtCall   Ident [Expr] SourcePos
    | ExtUncall Ident [Expr] SourcePos
    | UserError String SourcePos
    | Swap      Lval Lval SourcePos
    | Prints    Prints SourcePos
    -- | ShowVar   Ident SourcePos
    -- | ReadVar   Ident SourcePos
    | Skip      SourcePos
    | Assert    Expr SourcePos
    | Debug     DebugType SourcePos
    deriving (Eq)

data Argument
    = VarArg Ident
    | ArrayArg Ident [Ident]
    deriving (Eq)

-- Local Declaration
data LocalDecl
    = LocalVar DeclType Type Ident (Maybe Expr) SourcePos
    | LocalArray DeclType IntType Ident [Maybe Expr] (Maybe Expr) SourcePos
    deriving (Eq)

-- Expression
data Expr
    = Number   Integer SourcePos
    | Boolean  Bool SourcePos
    | LV       Lval SourcePos
    | UnaryOp  UnaryOp Expr
    | TypeCast Type Expr
    | BinOp    BinOp Expr Expr
    | Empty    Ident SourcePos
    | Top      Ident SourcePos
    | Size     Ident SourcePos
    | Nil      SourcePos
    | ArrayE   [Expr] SourcePos
    | Ternary  Expr Expr Expr
    deriving (Eq)

-- Declaration
data Vdecl
    = Scalar DeclType Type Ident (Maybe Expr) SourcePos
    | Array  DeclType IntType Ident [Maybe Expr] (Maybe Expr) SourcePos
    deriving (Eq)

data DeclType
    = Variable
    | Ancilla
    | Constant
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
  ident (Ident idnt _) = idnt

instance Identifiable Lval where
  ident (Var idnt) = ident idnt
  ident (Lookup idnt _) = ident idnt

instance Identifiable Vdecl where
  ident (Scalar _ _ idnt _ _) = ident idnt
  ident (Array  _ _ idnt _ _ _) = ident idnt

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

