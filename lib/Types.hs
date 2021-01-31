{-|
Module      : Types
Description : Datatypes for the project
Author      : Abhaas Goyal
-}

module Types where

-- Lamda Calculus Types
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq, Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Read)

-- Word Search Types
type WordSearchGrid = [[ Char ]]
type GridWithPos = [(Posn, Char)]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

