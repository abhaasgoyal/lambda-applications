module LambdaAdv where

import Data.Maybe
import Data.List
import Types

-- | Take maximum of used variables and generate a new variable
newVarGen :: [Int] -> Int
newVarGen a = maximum a + 1

-- | Helper function for used variables
usedVarAbs :: LamExpr -> [Int]
usedVarAbs mainExpr = nub $ helpUsedVar mainExpr
  where
    helpUsedVar :: LamExpr -> [Int]
    helpUsedVar lamExpr = case lamExpr of
                         (LamVar x) -> [x]
                         (LamMacro _) -> []
                         (LamAbs i remExpr) -> i:helpUsedVar remExpr
                         (LamApp expr1 expr2) -> helpUsedVar expr1 ++ helpUsedVar expr2

-- | List of usedVariables
listUsedVar :: LamMacroExpr -> [Int]
listUsedVar (LamDef defs expr) = nub $ concatMap (usedVarAbs . snd) defs ++ usedVarAbs expr

-- | Main function for transforming in cps
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform lam@(LamDef defs expr) = LamDef (zip (map fst defs)
                                              (map (helpTransform newVar . snd) defs))
                                      (helpTransform newVar expr)
  where
    newVar :: Int
    newVar = newVarGen $ listUsedVar lam

-- | Given a newVariable and an expression convert to cps
helpTransform :: Int -> LamExpr -> LamExpr
helpTransform n (LamVar x) = LamAbs n (LamApp (LamVar n) (LamVar x))
helpTransform n (LamAbs i expr) = LamAbs n (LamApp (LamVar n)
                                                   (LamAbs i
                                                    (helpTransform (n+1) expr)))
helpTransform n (LamApp expr1 expr2) = LamAbs n
                                       (LamApp (helpTransform (n+1) expr1)
                                         (LamAbs (n+2)
                                           (LamApp (helpTransform (n+3) expr2)
                                             (LamAbs (n+4) (LamApp
                                                             (LamApp (LamVar (n+2))
                                                               (LamVar (n+4)))
                                                             (LamVar n))))))
helpTransform _ (LamMacro x) = LamMacro x

-- Challenge 6

-- | Renaming variable x with y
rename :: Int -> Int -> LamExpr -> LamExpr
rename x y (LamVar z)
  | z == x = LamVar y
  | otherwise = LamVar z
rename x y (LamAbs z m)
  | z == x = LamAbs z m
  | otherwise = LamAbs z (rename x y m)
rename x y (LamApp m n) = LamApp (rename x y m) (rename x y n)
rename _ _ lamMacro = lamMacro

-- | Capture avoiding substitution of variable x with expression n
substituteVar :: Int -> LamExpr -> LamExpr -> LamExpr
substituteVar x n (LamVar y)
  | y == x = n
  | otherwise = LamVar y
substituteVar x n (LamAbs y m)
  | y == x = LamAbs y m
  | otherwise = LamAbs genZ (substituteVar x n $ rename y genZ m)
  where
    genZ :: Int
    genZ = (checkZ . nub) $ usedVarAbs m ++ usedVarAbs n

    checkZ :: [Int] -> Int
    checkZ list
      | newZ list == x = newZ (x:list)
      | otherwise = newZ list

    newZ :: [Int] -> Int
    newZ list = newVarGen list
substituteVar x n (LamApp m1 m2) = LamApp (substituteVar x n m1) (substituteVar x n m2)
substituteVar _ _ lamMacro = lamMacro


-- | Expands the macro in lambda expression given a macrolist
checkMacro :: [(String,LamExpr)] -> LamExpr -> [LamExpr]
checkMacro macros (LamMacro m)
  | isJust foundMacro = [fromJust foundMacro]
  | otherwise = []
  where
    foundMacro :: Maybe LamExpr
    foundMacro = lookup m macros
checkMacro _ _ = []


-- | Inner reduction helper
helpInner :: [(String, LamExpr)] -> LamExpr -> [LamExpr]
helpInner macros (LamApp (LamAbs z n) m) =
                                       [LamApp (LamAbs z n) ys | ys <- helpInner macros m] ++
                                       [LamApp (LamAbs z xs) m | xs <- helpInner macros n] ++
                                       [LamApp (LamAbs z zs) m | zs <- checkMacro macros n] ++
                                       [LamApp (LamAbs z n) zs | zs <- checkMacro macros m] ++
                                       [substituteVar z m n]

helpInner macros (LamApp n m) =
                            [LamApp xs m | xs <- helpInner macros n] ++
                            [LamApp n ys | ys <- helpInner macros m]

helpInner macros (LamAbs n m) = [LamAbs n xs | xs <- helpInner macros m]
helpInner macros expr@(LamMacro m) = checkMacro macros expr
helpInner macros _ = []

-- | Main inner reduction function
innerRedn1 :: LamMacroExpr ->  Maybe LamMacroExpr
innerRedn1 (LamDef macros expr)
  | null reducedExp = Nothing
  | otherwise = Just (LamDef macros (head reducedExp))
  where
    reducedExp = helpInner macros expr

-- To expand macro before applying

-- | Outer reduction helper
helpOuter :: [(String, LamExpr)] -> LamExpr -> [LamExpr]
helpOuter macros (LamApp (LamAbs z n) m) =
                                    [LamApp (LamAbs z zs) m | zs <- checkMacro macros n] ++
                                    [LamApp (LamAbs z n) zs | zs <- checkMacro macros m] ++
                                    [substituteVar z m n] ++
                                    [LamApp (LamAbs z n) ys | ys <- helpOuter macros m] ++
                                    [LamApp (LamAbs z xs) m | xs <- helpOuter macros n]

helpOuter macros (LamApp n m) =
                            [LamApp xs m | xs <- helpOuter macros n] ++
                            [LamApp n ys | ys <- helpOuter macros m]

helpOuter macros (LamAbs n m) = [LamAbs n xs | xs <- helpOuter macros m]
helpOuter macros expr@(LamMacro m) = checkMacro macros expr
helpOuter macros _ = []

-- | Main outer reduction function
outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef macros expr)
  | null reducedExp = Nothing
  | otherwise = Just (LamDef macros (head reducedExp))
  where
    reducedExp = helpOuter macros expr

-- | Identity application in a lamda expression
applyId :: LamMacroExpr -> LamMacroExpr
applyId (LamDef defs expr) = LamDef defs (LamApp expr (LamAbs 1 (LamVar 1)))

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter mainExp bound = (reducedExp innerRedn1 mainExp,
                                   reducedExp outerRedn1 mainExp,
                                   reducedExp innerRedn1 cpsExp,
                                   reducedExp outerRedn1 cpsExp)
  where

    cpsExp :: LamMacroExpr
    cpsExp = applyId . cpsTransform $ mainExp

    reducedExp :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr ->  Maybe Int
    reducedExp redStrat expr = checkBounds . length . take (bound + 2) . takeWhile isJust
                               $ iterate (maybe Nothing redStrat) (Just expr)

    checkBounds :: Int -> Maybe Int
    checkBounds a
      | a > bound + 1 = Nothing
      | otherwise = Just (a - 1)
