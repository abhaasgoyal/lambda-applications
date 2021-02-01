{-|
Module      : LambdaCalc
Description : Basic functions for printing expressions and parsing strings to expressions
Author      : Abhaas Goyal
-}
module LambdaCalc (prettyPrint, parseLamMacro, exprToMac) where
import Data.List
import Data.Maybe
import Parsing
import Types

-- | Pure Lamda expressions represented in macro form
exprToMac :: LamExpr -> LamMacroExpr
exprToMac = LamDef []

printVar :: Int -> String
printVar i = 'x' : show i

-- | prettyPrint LamAbsVar
printAbs :: Int -> String
printAbs i = "\\" ++ printVar i ++ "->"

-- | Given an expression and macros return pretty printed string
abstractExpr :: LamExpr -> [(String, LamExpr)] -> String
abstractExpr expr macros = case expr of
  LamVar x -> printVar x
  LamAbs x remExpr -> printAbs x ++ abstractExpr remExpr macros
  LamMacro str -> str
  LamApp expr1 expr2@(LamApp _ _) -> printExpr expr1 ++ "(" ++ abstractExpr expr2 macros ++ ")"
  LamApp expr1@(LamAbs _ _) expr2 -> printBracketedExpr expr1 ++ " " ++ abstractExpr expr2 macros
  LamApp expr1 expr2 -> printExpr expr1 ++ " " ++ printExpr expr2
  where
    printBracketedExpr :: LamExpr -> String
    printBracketedExpr remExpr
      | isNothing $ advLookUpExpr remExpr = "(" ++ printExpr remExpr ++ ")"
      | otherwise = printExpr remExpr

    printExpr :: LamExpr -> String
    printExpr remExpr
      | isNothing $ advLookUpExpr remExpr = abstractExpr remExpr macros
      | otherwise = fst $ fromMaybe ("Illegal Macro", LamVar (-1)) $ advLookUpExpr remExpr

    advLookUpExpr :: LamExpr -> Maybe (String, LamExpr)
    advLookUpExpr exp1 = find (\(_, exp2) -> eqSyntaxExpr [] exp1 exp2) macros

-- | Checks whether 2 expressions are syntactically equal
eqSyntaxExpr :: [(Int, Int)] -> LamExpr -> LamExpr -> Bool
eqSyntaxExpr varLookUp expr1 expr2 = case (expr1, expr2) of
  (LamVar x, LamVar y) -> checkEq x y
  (LamApp e1 e2, LamApp e3 e4) -> eqSyntaxExpr varLookUp e1 e3 && eqSyntaxExpr varLookUp e2 e4
  -- If there exists another variable with same i1 nub's functionality will remove it
  -- So scoping is maintained
  (LamAbs i1 e1, LamAbs i2 e2) -> eqSyntaxExpr (nub $ (i1, i2) : varLookUp) e1 e2
  (LamMacro s1, LamMacro s2) -> s1 == s2
  _ -> False
  where
    -- Either a variable on lookup equal or a free variable
    checkEq :: Int -> Int -> Bool
    checkEq a b = lookUpInt a b || (isNothing (lookup a varLookUp) && lookUpInt a b)

    lookUpInt :: Int -> Int -> Bool
    lookUpInt a b = fromMaybe (-1) (lookup a varLookUp) == b

-- | Main function for pretty printing
prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef defs expr) = concatMap helpPrintDef defs ++ abstractExpr expr defs

helpPrintDef :: (String, LamExpr) -> String
helpPrintDef (macro, expr) = "def " ++ macro ++ "=" ++ abstractExpr expr [] ++ " in "

-- Challenge 4 --

-- | Following functions are parsing rules for various generations
parseMacroExpr :: Parser LamMacroExpr
parseMacroExpr =
  do
    def <- parseDef
    (LamDef defs expr) <- parseMacroExpr
    return (LamDef (def : defs) expr)
    <|> LamDef [] <$> parseExpr

parseDef :: Parser (String, LamExpr)
parseDef = do
  string "def"
  minSpace
  macro <- parseMacro
  let (LamMacro x) = macro
  space
  char '='
  space
  expr <- parseExpr
  minSpace
  string "in"
  minSpace
  return (x, expr)

parseExpr :: Parser LamExpr
parseExpr = parseApp <|> parseExprWOApp

parseExprWOApp :: Parser LamExpr
parseExprWOApp = parseVar <|> parseMacro <|> parseAbs <|> parseBrac

parseVar :: Parser LamExpr
parseVar = do
  char 'x'
  LamVar <$> nat

parseMacro :: Parser LamExpr
parseMacro = LamMacro <$> some upper

parseApp :: Parser LamExpr
parseApp =
  do
    space
    expr1 <- parseExprWOApp
    space
    expr2 <- parseExprWOApp
    do
      space
      LamApp (LamApp expr1 expr2) <$> parseExpr
      <|> return (LamApp expr1 expr2)
    <|> parseExprWOApp

parseAbs :: Parser LamExpr
parseAbs = do
  symbol "\\"
  lamVar <- parseVar
  let (LamVar var) = lamVar
  symbol "->"
  LamAbs var <$> parseExpr

parseBrac :: Parser LamExpr
parseBrac = do
  space
  char '('
  expr <- parseExpr
  char ')'
  return expr

-- | Main function for parsing macro expressions
parseLamMacro :: String -> Either String LamMacroExpr
parseLamMacro str = case parse parseMacroExpr str of
  [] -> Left "Invalid grammar input"
  (x, "") : _ -> checkMacroExpr x
  _ -> Left "Half parsed string"

-- | Additional checks for repeated macro names or macros having free variables
checkMacroExpr :: LamMacroExpr -> Either String LamMacroExpr
checkMacroExpr expr@(LamDef defs _)
  | nub macroStrList /= macroStrList = Left "Repeated macro definition"
  | not macroCheckScope = Left "Free variable in macro"
  | otherwise = Right expr
  where
    macroStrList :: [String]
    macroStrList = map fst defs

    macroCheckScope :: Bool
    macroCheckScope = all (checkScope [] . snd) defs

checkScope :: [Int] -> LamExpr -> Bool
checkScope xs expr = case expr of
  (LamVar x) -> x `elem` xs
  (LamMacro _) -> True
  (LamAbs i remExpr) -> checkScope (i : xs) remExpr
  (LamApp expr1 expr2) ->
    checkScope xs expr1
      && checkScope xs expr2
