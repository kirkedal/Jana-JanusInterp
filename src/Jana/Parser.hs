module Jana.Parser (
    parseExprString, parseStmtsString, parseString, parseFile,
    parseProgram, parseStmts
    ) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr
import qualified Text.Parsec.Error as ParsecError
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity (..))

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

janaDef :: Token.GenLanguageDef String u Identity
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
                                         , "external"
                                         , "error"
                                         , "skip"
                                         , "empty"
                                         , "top"
                                         , "size"
                                         , "show"
                                         , "print"
                                         , "printf"
                                         , "nil"
                                         , "assert"
                                         , "iterate"
                                         , "by"
                                         , "to"
                                         ]
              , Token.caseSensitive    = True
  }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser janaDef

t_identifier :: Parser String
t_identifier = Token.identifier lexer -- parses an identifier
reserved :: String -> Parser ()
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit :: Parser String
stringLit  = Token.stringLiteral lexer
parens :: Parser a -> Parser a
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
brackets :: Parser a -> Parser a
brackets   = Token.brackets   lexer -- parses brackets
braces :: Parser a -> Parser a
braces     = Token.braces     lexer -- parses brackets
integer :: Parser Integer
integer    = Token.integer    lexer -- parses an integer
comma :: Parser String
comma      = Token.comma      lexer -- parses a comma
symbol :: String -> Parser String
symbol     = Token.symbol     lexer
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

identifier :: Parser Ident
identifier =
  do pos   <- getPosition
     idnt <- t_identifier
     return $ Ident idnt pos

program :: Parser Program
program =
  do whiteSpace
     (mains, procs) <- liftM partitionEithers (many genProcedure)
     eof
     case mains of
       []  -> return $ Program Nothing procs
       [m] -> return $ Program (Just m) procs
       _   -> error "More than one mail procedure defined"

genProcedure :: Parser (Either ProcMain Proc)
genProcedure =
  do reserved "procedure"
     idnt <- identifier
     case idnt of
       (Ident "main" pos) -> liftM Left  $ mainProcedure pos
       _                  -> liftM Right $ procedure idnt

mainProcedure :: SourcePos -> Parser ProcMain
mainProcedure pos =
  do parens whiteSpace
     vdecls <- many mainvdecl
     stats  <- many1 statement
     return $ ProcMain vdecls stats pos

mainvdecl :: Parser Vdecl
mainvdecl =
  do pos    <- getPosition
     mytype <- atype
     idnt  <- identifier
     case mytype of
       (Int _) -> liftM2 (\x y -> (Array idnt x y pos)) (many1 $ brackets $ optionMaybe expression) (optionMaybe $ reservedOp "=" >> array)
              <|> liftM (\x -> (Scalar mytype idnt x pos)) (optionMaybe $ reservedOp "=" >> expression)
       _       -> return $ (Scalar mytype idnt Nothing pos)

procedure :: Ident -> Parser Proc
procedure name =
  do pars  <- parens $ sepBy vdecl comma
     stats <- many1 statement
     return Proc { procname = name, params = pars, body = stats }

vdecl :: Parser Vdecl
vdecl =
  do pos    <- getPosition
     mytype <- atype
     idnt  <- identifier
     case mytype of
       (Int _) -> liftM3 (Array idnt) (many1 $ brackets $ optionMaybe expression) (return Nothing) (return pos)
              <|> return (Scalar mytype idnt Nothing pos)
       _       -> return $ Scalar mytype idnt Nothing pos

statement :: Parser Stmt
statement =   assignStmt
          <|> ifStmt
          <|> fromStmt
          <|> iterateStmt
          <|> pushStmt
          <|> popStmt
          <|> localStmt
          <|> callStmt
          <|> uncallStmt
          <|> swapStmt
          <|> errorStmt
          <|> printsStmt
          <|> skipStmt
          <|> assertStmt
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

iterateStmt :: Parser Stmt
iterateStmt =
  do pos   <- getPosition
     reserved "iterate"
     typ   <- atype
     idnt <- identifier
     from  <- reservedOp "=" >> expression
     step  <- option (Number 1 pos) $ reserved "by" >> expression
     end   <- reserved "to" >> expression
     stm   <- many1 statement
     reserved "end"
     return $ Iterate typ idnt from step end stm pos

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
         return $ Local (LocalVar (Int p) f expr p) [stmtFun f] (LocalVar (Int p) f (Number 0p ) p) p

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
         return $ Local (LocalVar (Int p) f (Number 0 p) p) [stmtFun f] (LocalVar (Int p) f expr p) p

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
      do pos    <- getPosition
         typ    <- atype
         idnt  <- identifier
         case typ of
           (Int _) -> liftM2 (\x y -> (LocalArray idnt x y pos)) (many1 $ brackets $ optionMaybe expression) (reservedOp "=" >> array)
                  <|> liftM  (\x -> (LocalVar typ idnt x pos)) (reservedOp "=" >> expression)
           _       -> liftM  (\x -> (LocalVar typ idnt x pos)) (reservedOp "=" >> expression)

atype :: Parser Type
atype =   (reserved "int"   >> liftM Int getPosition)
      <|> (reserved "stack" >> liftM Stack getPosition)

callStmt :: Parser Stmt
callStmt =
  do pos   <- getPosition
     reserved "call"
     e <- optionMaybe $ reserved "external"
     pname <- identifier
     args_exp <- parens $ sepBy expression comma
     formatArgumentList args_exp (callType e pname pos)
  where
    callType Nothing  pname pos = (\a -> Call pname a pos)
    callType (Just _) pname pos = (\a -> ExtCall pname a pos)

uncallStmt :: Parser Stmt
uncallStmt =
  do pos   <- getPosition
     reserved "uncall"
     e <- optionMaybe $ reserved "external"
     pname <- identifier
     args_exp <- parens $ sepBy expression comma
     formatArgumentList args_exp (callType e pname pos)
  where
    callType Nothing  pname pos = (\a -> Uncall pname a pos)
    callType (Just _) pname pos = (\a -> ExtUncall pname a pos)

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
    foldFun _  Nothing    stmt = stmt
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

assertStmt :: Parser Stmt
assertStmt = reserved "assert" >> liftM2 Assert expression getPosition

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
  do idnt <- identifier
     lexpr <- optionMaybe (many1 $ brackets expression)
     case lexpr of
       Just exprs -> return $ Lookup idnt exprs
       Nothing   -> return $ Var idnt

nilExpr :: Parser (SourcePos -> Expr)
nilExpr = reserved "nil" >> return Nil

emptyExpr :: Parser (SourcePos -> Expr)
emptyExpr =
  do reserved "empty"
     idnt <- parens identifier
     return $ Empty idnt

topExpr :: Parser (SourcePos -> Expr)
topExpr =
  do reserved "top"
     idnt <- parens identifier
     return $ Top idnt

sizeExpr :: Parser (SourcePos -> Expr)
sizeExpr = reserved "size" >> liftM Size (parens identifier)

binOperators :: [[Operator String Int Identity Expr]]
binOperators = [ [ notChain
                 ]
               , [ binop  "**"  Exp
                 ]
               , [ binop' "*"   Mul '*'
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
  where binop  op f   = Infix (reservedOp op >> return (BinOp f)) AssocLeft
        binop' op f n = Infix (try $ reservedOp op >> notFollowedBy (char n) >>
                                       return (BinOp f)) AssocLeft
        notChain      = Prefix $ chainl1 notOp $ return (.)
        notOp         = symbol "!" >> return (UnaryOp Not)

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

parseStmts :: SourcePos -> String -> String -> Either JanaError [Stmt]
parseStmts pos filename text =
  case runParser stmtParser 0 filename text of
    Left e  -> Left $ toJanaError e
    Right r -> Right r
  where 
    stmtParser =
      do
        setPosition pos 
        s <- many statement
        eof
        return s
