
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

module RevTM.Types
  where

import Jana.Ast
import Text.Parsec.Pos

type RevTMProg = [RevTMBlock]

type TMVar   = String
type TMInput = String

data RevTMBlock = 
    Csect String
  | RevTMsect [TMInput] [TMVar] String SourcePos
  deriving (Eq)

