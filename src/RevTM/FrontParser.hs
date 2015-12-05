-----------------------------------------------------------------------------
--
-- Module      :  FrontParser
-- Copyright   :  Michael Kirkedal Thomsen, 2016
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- Comment     :  Implemented mostly by Oliver Keszöcze
--
-- |Parses an FDL program into an "FDL.Ast"
--
-----------------------------------------------------------------------------

module RevTM.FrontParser (
    parseString, parseFile
    ) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Text.Parsec hiding (Empty)
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Error
import qualified Text.Parsec.Token as Token

import Jana.Ast
import RevTM.Types

-- The Int is indented as used for defining fresh variables.
type Parser = Parsec String Int

-- toParserError :: ParsecError.ParseError -> ParserError
-- toJanaError err =
--   newErrorMessage pos (Message $ trim msg)
--   where pos = ParsecError.errorPos err
--         msg = ParsecError.showErrorMessages
--                 "or" "Unknown parse error"
--                 "Expecting" "Unexpected" "end of input"
--                 (ParsecError.errorMessages err)
--         trim = dropWhile isSpace


revTMDef = Token.LanguageDef {
                Token.commentStart     = ""
              , Token.commentEnd       = ""
              , Token.commentLine      = ""
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_' <|> char '\''
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "JANUS_TM_BEGIN"
                                         , "JANUS_TM_END"
                                         , "JANUS_TM_VARS"
                                         ]
              , Token.caseSensitive    = True
  }

lexer = Token.makeTokenParser revTMDef

identifier = Token.identifier lexer -- parses an identifier
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


rTMBegin = reserved "JANUS_TM_BEGIN"
rTMVars  = reserved "JANUS_TM_VARS"
rTMEnd   = reserved "JANUS_TM_END"

-- program :: Parser Program
-- program =
--   do whiteSpace
--      (mains, procs) <- liftM partitionEithers (many genProcedure)
--      eof
--      return $ Program mains procs

program :: Parser RevTMProg
program = liftM concat $ many1 tmBlock

tmBlock :: Parser [RevTMBlock]
tmBlock = try cSect <|> cSectEnd
    where
        cSect = 
          do lookAhead anyChar
             str    <- anyChar `manyTill` (try $ rTMBegin )
             inputs <- parens (identifier `sepBy` comma)
             semi
             vars   <- option [] cTMVars
             pos    <- getPosition
             jstr   <- anyChar `manyTill` (try rTMEnd)
             parens whiteSpace
             semi
             return $ [Csect str, RevTMsect inputs vars jstr pos]
        cTMVars = 
          do rTMVars
             vars <- parens (identifier `sepBy` comma)
             semi
             return vars
        cSectEnd = 
          do s <- anyChar
             str <- anyChar `manyTill` (eof)
             return $ [Csect (s:str)]


parseFile :: String -> IO (Either ParseError RevTMProg)
parseFile file =
  do str <- readFile file
     case runParser program 0 file str of
       Left e  -> return $ Left  e
       Right r -> return $ Right r

parseString :: String -> String -> Either ParseError RevTMProg
parseString filename text =
  case runParser program 0 filename text of
    Left e  -> Left  e
    Right r -> Right r










