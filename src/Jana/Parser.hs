module Jana.Parser (
    parseExprString, parseStmtsString, parseString, parseFile,
    parseProgram
    ) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (liftM, liftM2)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.List (cycle)
import Text.Parsec hiding (Empty)
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Error as ParsecError
import qualified Text.Parsec.Token as Token

import Jana.Ast
import Jana.Error

-- The Int is indented as used for defining fresh variables.
type Parser = Parsec String Int

getFreshVar :: Parser Ident
getFreshVar = 
  do i <- getState
     putState $ i + 1
     p <- getPosition
     return $ Ident ("_parse_tmp_" ++ show i) p

toJanaError :: ParsecError.ParseError -> JanaError
toJanaError err =
  newErrorMessage pos (Message $ trim msg)
  where pos = ParsecError.errorPos err
        msg = ParsecError.showErrorMessages
                "or" "Unknown parse error"
                "Expecting" "Unexpected" "end of input"
                (ParsecError.errorMessages err)
        trim = dropWhile isSpace


janaDef = Token.LanguageDef {
                Token.commentStart     = "/*"
              , Token.commentEnd       = "*/"
              , Token.commentLine      = "//"
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "procedure"
                                         , "int"
                                         , "stack"
                                         , "bool"
                                         , "true"
                                         , "false"
                                         , "if"
                                         , "then"
                                         , "else"
                                         , "fi"
                                         , "from"
                                         , "do"
                                         , "loop"
                                         , "until"
                                         , "push"
                                         , "pop"
                                         , "local"
                                         , "delocal"
                                         , "call"
                                         , "uncall"
                                         , "error"
                                         , "skip"
                                         , "empty"
                                         , "top"
                                         , "size"
                                         , "show"
                                         , "print"
                                         , "printf"
                                         , "nil"
                                         ]
              , Token.caseSensitive    = True
  }

lexer = Token.makeTokenParser janaDef

t_identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit  = Token.stringLiteral lexer
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
brackets   = Token.brackets   lexer -- parses brackets
braces     = Token.braces     lexer -- parses brackets
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
comma      = Token.comma      lexer -- parses a comma
symbol     = Token.symbol     lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

identifier :: Parser Ident
identifier =
  do pos   <- getPosition
     ident <- t_identifier
     return $ Ident ident pos

program :: Parser Program
program =
  do whiteSpace
     (mains, procs) <- liftM partitionEithers (many genProcedure)
     eof
     return $ Program mains procs

genProcedure :: Parser (Either ProcMain Proc)
genProcedure =
  do reserved "procedure"
     id <- identifier
     case id of
       (Ident "main" pos) -> liftM Left  $ mainProcedure pos
       _                  -> liftM Right $ procedure id

mainProcedure :: SourcePos -> Parser ProcMain
mainProcedure pos =
  do parens whiteSpace
     (vdecls, vassings) <- liftM unzip $ many mainvdecl
     stats  <- many1 statement
     let assigns = concatMap (\(id, declv) -> makeAssign id declv) $ catMaybes vassings
     return $ ProcMain vdecls (assigns ++ stats) pos
  where
    makeAssign id (VarDecl            expr)  = [Assign AddEq (Var id) expr pos]
    makeAssign id (ArrayDecl Nothing  exprs) = []
    makeAssign id (ArrayDecl (Just i) exprs) = map (\(e,j) -> Assign AddEq (Lookup id (Number j pos)) e pos) $ zip (cycle exprs) [0..i-1]

mainvdecl :: Parser (Vdecl, Maybe (Ident, DeclVal))
mainvdecl =
  do pos    <- getPosition
     mytype <- atype
     ident  <- identifier
     case mytype of
       (Int _) -> liftM2 (\x y -> (Array ident x pos, extendMaybe y (\v -> ArrayDecl x v) ident)) (brackets $ optionMaybe integer) (optionMaybe $ reservedOp "=" >> braces (sepBy1 expression comma))
              <|> liftM (\x -> (Scalar mytype ident pos, extendMaybe x (\v -> VarDecl v) ident)) (optionMaybe $ reservedOp "=" >> expression)
       _       -> return $ (Scalar mytype ident pos, Nothing)
  where
    extendMaybe Nothing  _ _ = Nothing
    extendMaybe (Just a) f b = Just (b,f a)

procedure :: Ident -> Parser Proc
procedure name =
  do params <- parens $ sepBy vdecl comma
     stats  <- many1 statement
     return Proc { procname = name, params = params, body = stats }

vdecl :: Parser Vdecl
vdecl =
  do pos    <- getPosition
     mytype <- atype
     ident  <- identifier
     case mytype of
       (Int _) ->     liftM2 (Array ident) (brackets $ optionMaybe integer) (return pos)
                  <|> return (Scalar mytype ident pos)
       _       -> return $ Scalar mytype ident pos

statement :: Parser Stmt
statement =   assignStmt
          <|> ifStmt
          <|> fromStmt
          <|> pushStmt
          <|> popStmt
          <|> localStmt
          <|> callStmt
          <|> uncallStmt
          <|> swapStmt
          <|> errorStmt
          <|> printsStmt
          <|> skipStmt
          <?> "statement"

assignStmt :: Parser Stmt
assignStmt =
  do assign <- try $
       do lv    <- lval
          pos   <- getPosition
          modop <- modOp
          return $ flip (Assign modop lv) pos
     expr  <- expression
     return $ assign expr

modOp :: Parser ModOp
modOp =   (reservedOp "+=" >> return AddEq)
      <|> (reservedOp "-=" >> return SubEq)
      <|> (reservedOp "^=" >> return XorEq)

ifStmt :: Parser Stmt
ifStmt =
  do pos   <- getPosition
     reserved "if"
     entrycond <- expression
     reserved "then"
     stats1    <- many1 statement
     stats2    <- option [] $ reserved "else" >> many1 statement
     reserved "fi"
     exitcond  <- expression
     return $ If entrycond stats1 stats2 exitcond pos

fromStmt :: Parser Stmt
fromStmt =
  do pos   <- getPosition
     reserved "from"
     entrycond <- expression
     stats1    <- option [] $ reserved "do"   >> many1 statement
     stats2    <- option [] $ reserved "loop" >> many1 statement
     reserved "until"
     exitcond  <- expression
     return $ From entrycond stats1 stats2 exitcond pos

pushStmt :: Parser Stmt
pushStmt =
  do pos   <- getPosition
     reserved "push"
     (x,y) <- parens twoArgs
     chkExpression (\z -> Push z y pos) x
  where
    chkExpression stmtFun (LV (Var i) _) = return $ stmtFun i
    chkExpression stmtFun expr = 
      do f <- getFreshVar
         p <- getPosition
         return $ Local (Int p, f, expr) [stmtFun f] (Int p, f, Number 0 p) p

popStmt :: Parser Stmt
popStmt =
  do pos   <- getPosition
     reserved "pop"
     (x,y) <- parens twoArgs
     chkExpression (\z -> Pop z y pos) x
  where
    chkExpression stmtFun (LV (Var i) _) = return $ stmtFun i
    chkExpression stmtFun expr = 
      do f <- getFreshVar
         p <- getPosition
         return $ Local (Int p, f, Number 0 p) [stmtFun f] (Int p, f, expr) p

twoArgs :: Parser (Expr, Ident)
twoArgs =
  do x <- expression
     comma
     y <- identifier
     return (x,y)

localStmt :: Parser Stmt
localStmt =
  do pos      <- getPosition
     reserved "local"
     locs     <- decl `sepBy1` comma
     stats    <- many1 statement
     reserved "delocal"
     fstdeloc <- decl
     delocs   <- count (length locs-1) $ comma >> decl 
     let alllocs = zip locs $ fstdeloc:delocs
     return $ head $ foldr (\(x,y) s -> [Local x s y pos]) stats alllocs
  where 
    decl = 
      do typ    <- atype
         ident  <- identifier
         reservedOp "="
         expr   <- expression
         return (typ, ident, expr)



atype :: Parser Type
atype =   (reserved "int"   >> liftM Int getPosition)
      <|> (reserved "stack" >> liftM Stack getPosition)

callStmt :: Parser Stmt
callStmt =
  do pos   <- getPosition
     reserved "call"
     procname <- identifier
     args_exp <- parens $ sepBy expression comma
     formatArgumentList args_exp (\a -> Call procname a pos)

uncallStmt :: Parser Stmt
uncallStmt =
  do pos   <- getPosition
     reserved "uncall"
     procname <- identifier
     args_exp <- parens $ sepBy expression comma
     formatArgumentList args_exp (\a -> Uncall procname a pos)

formatArgumentList :: [Expr] -> ([Ident] -> Stmt) -> Parser Stmt
formatArgumentList args_expr stmtFun =
  do pos   <- getPosition
     args_map <- mapM chkExpression args_expr
     let (args,exprs) = unzip args_map 
     return $ foldr (foldFun pos) (stmtFun args) exprs
  where
    chkExpression (LV (Var i) _) = return (i, Nothing)
    chkExpression expr = 
      do f <- getFreshVar
         return (f, Just((f,expr)))
    foldFun p  Nothing    stmt = stmt
    foldFun p (Just(i,e)) stmt = Local (Int p, i, e) [stmt] (Int p, i, e) p

swapStmt :: Parser Stmt
swapStmt =
  do swap <- try $
       do ident1 <- lval
          pos    <- getPosition
          reservedOp "<=>"
          return $ flip (Swap ident1) pos
     ident2 <- lval
     return $ swap ident2

errorStmt :: Parser Stmt
errorStmt =
  do pos <- getPosition
     reserved "error"
     liftM (flip UserError pos) (parens stringLit)

printsStmt :: Parser Stmt
printsStmt =   liftM2 (flip Prints) getPosition printStmt
           <|> liftM2 (flip Prints) getPosition printfStmt
           <|> liftM2 (flip Prints) getPosition showStmt

printStmt :: Parser Prints
printStmt =
  do reserved "print"
     str <- parens stringLit
     return $ Print str

printfStmt :: Parser Prints
printfStmt =
  do reserved "printf"
     (a, b) <- parens $ do str <- stringLit
                           identList <- option [] $ comma >> sepBy1 identifier comma
                           return (str, identList)
     return $ Printf a b

showStmt :: Parser Prints
showStmt =
  do reserved "show"
     identList <- parens $ sepBy1 identifier comma
     return $ Show identList

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> liftM Skip getPosition

expression :: Parser Expr
expression = buildExpressionParser binOperators term

addPos :: Parser (SourcePos -> a) -> Parser a
addPos parser = do pos <- getPosition
                   t   <- parser
                   return $ t pos

term :: Parser Expr
term =   parens expression
     <|> addPos numberExpr
     <|> addPos boolExpr
     <|> addPos lvalExpr
     <|> addPos emptyExpr
     <|> addPos topExpr
     <|> addPos sizeExpr
     <|> addPos nilExpr
     <?> "expression"

numberExpr :: Parser (SourcePos -> Expr)
numberExpr = liftM Number integer

boolExpr :: Parser (SourcePos -> Expr)
boolExpr =   (reserved "true"  >> return (Boolean True))
         <|> (reserved "false" >> return (Boolean False))

lvalExpr :: Parser (SourcePos -> Expr)
lvalExpr = liftM LV lval

lval ::  Parser Lval
lval =
  do ident <- identifier
     lookup <- optionMaybe (brackets expression)
     case lookup of
       Just expr -> return $ Lookup ident expr
       Nothing   -> return $ Var ident

nilExpr :: Parser (SourcePos -> Expr)
nilExpr = reserved "nil" >> return Nil

emptyExpr :: Parser (SourcePos -> Expr)
emptyExpr =
  do reserved "empty"
     ident <- parens identifier
     return $ Empty ident

topExpr :: Parser (SourcePos -> Expr)
topExpr =
  do reserved "top"
     ident <- parens identifier
     return $ Top ident

sizeExpr :: Parser (SourcePos -> Expr)
sizeExpr = reserved "size" >> liftM Size (parens identifier)

binOperators = [ [ notChain
                 ]
               , [ binop  "*"   Mul
                 , binop  "/"   Div
                 , binop  "%"   Mod
                 ]
               , [ binop  "+"   Add
                 , binop  "-"   Sub
                 ]
               , [ binop  "<="  LE
                 , binop  "<"   LT
                 , binop  ">="  GE
                 , binop  ">"   GT
                 , binop  "="   EQ
                 , binop  "!="  NEQ
                 ]
               , [ binop' "&"   And '&'
                 , binop' "|"   Or  '|'
                 , binop  "^"   Xor
                 ]
               , [ binop  "&&"  LAnd
                 , binop  "||"  LOr
                 ]
               ]
  where binop  op f     = Infix (reservedOp op >> return (BinOp f)) AssocLeft
        binop' op f not = Infix (try $ reservedOp op >> notFollowedBy (char not) >>
                                       return (BinOp f)) AssocLeft
        notChain        = Prefix $ chainl1 notOp $ return (.)
        notOp           = symbol "!" >> return (UnaryOp Not)

parseString :: Parser a -> String -> a
parseString parser str =
  case runParser parser 0 "" str of
    Left e  -> error $ show e
    Right r -> r

parseExprString :: String -> Expr
parseExprString = parseString expression

parseStmtsString :: String -> [Stmt]
parseStmtsString = parseString (many1 statement)

parseFile :: String -> IO (Either JanaError Program)
parseFile file =
  do str <- readFile file
     case runParser program 0 file str of
       Left e  -> return $ Left $ toJanaError e
       Right r -> return $ Right r

parseProgram :: String -> String -> Either JanaError Program
parseProgram filename text =
  case runParser program 0 filename text of
    Left e  -> Left $ toJanaError e
    Right r -> Right r
