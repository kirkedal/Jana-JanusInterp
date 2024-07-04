module Jana.ParserBasic (
    parseExprString, parseStmtsString, parseString, parseFile,
    parseProgram
    ) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
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
              , Token.identLetter      = alphaNum <|> char '_' <|> char '\''
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "procedure"
                                         -- , "int"
                                         -- , "true"
                                         -- , "false"
                                         , "if"
                                         , "then"
                                         , "else"
                                         , "fi"
                                         , "from"
                                         , "do"
                                         , "loop"
                                         , "until"
                                         , "call"
                                         , "uncall"
                                         , "skip"
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
     globs <- many global
     (mains, procs) <- liftM partitionEithers (many $ genProcedure globs)
     eof
     return $ Program mains procs

global :: Parser Vdecl
global = 
  do (try glob_array) <|> glob_var
  where
    glob_var = 
      do 
        p <- getPosition
        i <- identifier
        return $ Scalar (Int p) i Nothing p
    glob_array = 
      do 
        p <- getPosition
        i <- identifier
        s <- brackets integer
        return $ Array i [Just (Number s p)] Nothing p



genProcedure :: [Vdecl] -> Parser (Either ProcMain Proc)
genProcedure globs =
  do reserved "procedure"
     id <- identifier
     case id of
       (Ident "main" pos) -> liftM Left  $ mainProcedure globs pos
       _                  -> liftM Right $ procedure globs id

mainProcedure :: [Vdecl] -> SourcePos -> Parser ProcMain
mainProcedure globs pos =
  do 
     stats  <- many1 $ statement (map identsFromVdecl globs)
     return $ ProcMain globs stats pos
  where
    identsFromVdecl (Scalar _ i _ _) = i
    identsFromVdecl (Array  i _ _ _) = i

-- mainvdecl :: Parser Vdecl
-- mainvdecl =
--   do pos    <- getPosition
--      mytype <- atype
--      ident  <- identifier
--      case mytype of
--        (Int _) -> liftM2 (\x y -> (Array ident x y pos)) (many1 $ brackets $ optionMaybe expression) (optionMaybe $ reservedOp "=" >> array)
--               <|> liftM (\x -> (Scalar mytype ident x pos)) (optionMaybe $ reservedOp "=" >> expression)
--        _       -> return $ (Scalar mytype ident Nothing pos)
--   where
--     extendMaybe Nothing  _ _ = Nothing
--     extendMaybe (Just a) f b = Just (b,f a)

procedure :: [Vdecl] -> Ident -> Parser Proc
procedure globs name =
  do 
     stats  <- many1 $ statement (map identsFromVdecl globs)
     return Proc { procname = name, params = globs, body = stats }
  where
    identsFromVdecl (Scalar _ i _ _) = i
    identsFromVdecl (Array  i _ _ _) = i

-- vdecl :: Parser Vdecl
-- vdecl =
--   do pos    <- getPosition
--      mytype <- atype
--      ident  <- identifier
--      case mytype of
--        (Int _) -> liftM3 (Array ident) (many1 $ brackets $ optionMaybe expression) (return Nothing) (return pos)
--               <|> return (Scalar mytype ident Nothing pos)
--        _       -> return $ Scalar mytype ident Nothing pos

statement :: [Ident] -> Parser Stmt
statement idents =   assignStmt
          <|> ifStmt     idents
          <|> fromStmt   idents
          <|> callStmt   idents
          <|> uncallStmt idents
          <|> swapStmt
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
      <|> (reservedOp "*=" >> return MulEq)
      <|> (reservedOp "/=" >> return DivEq)
      <|> (reservedOp "**=" >> return ExpEq)
      <|> (reservedOp "//=" >> return RtEq)

ifStmt :: [Ident] -> Parser Stmt
ifStmt idents =
  do pos   <- getPosition
     reserved "if"
     entrycond <- expression
     reserved "then"
     stats1    <- many1 $ statement idents
     stats2    <- option [] $ reserved "else" >> many1 (statement idents)
     reserved "fi"
     exitcond  <- expression
     return $ If entrycond stats1 stats2 exitcond pos

fromStmt :: [Ident] -> Parser Stmt
fromStmt idents =
  do pos   <- getPosition
     reserved "from"
     entrycond <- expression
     stats1    <- option [] $ reserved "do"   >> many1 (statement idents)
     stats2    <- option [] $ reserved "loop" >> many1 (statement idents)
     reserved "until"
     exitcond  <- expression
     return $ From entrycond stats1 stats2 exitcond pos

twoArgs :: Parser (Expr, Ident)
twoArgs =
  do x <- expression
     comma
     y <- identifier
     return (x,y)

atype :: Parser Type
atype =   (reserved "int"   >> liftM Int getPosition)
      <|> (reserved "stack" >> liftM Stack getPosition)

callStmt :: [Ident] -> Parser Stmt
callStmt idents =
  do pos   <- getPosition
     reserved "call"
     procname <- identifier
     -- args_exp <- parens $ sepBy expression comma
     -- formatArgumentList args_exp (\a -> Call procname a pos)
     return $ Call procname idents pos

uncallStmt :: [Ident] -> Parser Stmt
uncallStmt idents =
  do pos   <- getPosition
     reserved "uncall"
     procname <- identifier
     -- args_exp <- parens $ sepBy expression comma
     -- formatArgumentList args_exp (\a -> Uncall procname a pos)
     return $ Uncall procname idents pos

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
    foldFun p (Just(i,e)) stmt = Local (LocalVar (Int p) i e p) [stmt] (LocalVar (Int p) i e p) p

swapStmt :: Parser Stmt
swapStmt =
  do swap <- try $
       do ident1 <- lval
          pos    <- getPosition
          reservedOp "<=>"
          return $ flip (Swap ident1) pos
     ident2 <- lval
     return $ swap ident2

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

array :: Parser Expr
array = liftM2 ArrayE (braces $ sepBy1 array comma) getPosition
    <|> expression
-- array = try $ liftM2 ArrayE (braces $ sepBy1 array comma) getPosition
--     <|> liftM2 ArrayE (braces $ sepBy1 expression comma) getPosition

lval ::  Parser Lval
lval =
  do ident <- identifier
     lookup <- optionMaybe (many1 $ brackets expression)
     case lookup of
       Just exprs -> return $ Lookup ident exprs
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
                 , binop  "**"  Exp
                 , binop  "*/"  Rt
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

parseStmtsString :: [Ident] -> String -> [Stmt]
parseStmtsString idents = parseString (many1 $ statement idents)

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
