{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter, correctGrid, applyId) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Data.Maybe
import Data.Tuple
import Parsing
import Control.Monad
import Data.List
import qualified Data.Ord
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type GridWithPos = [(Posn, Char)]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

-- END OF CODE YOU MUST NOT MODIFY



-- ADD YOUR OWN CODE HERE

-- Challenge 1 --
-- | Diagonal String out of grid
diagonalString :: [GridWithPos] -> GridWithPos
diagonalString wds = [ wds !! (x - y) !! y | x <- [0..(len - 1)] , y <- [0 .. x]] ++
                     [ wds !! y !! (len + x - y) | x <- [0..len] , y <- [len, (len - 1) .. x]]
  where
    len = length wds - 1

-- | Search for placement of one word in an oriented grid
search :: String -> (GridWithPos, Orientation) -> Maybe Placement
search word (grid, orient) = foldl searchCond Nothing searchGrid
  where
    len = length word

    searchCond :: Maybe Placement -> (Posn, String) -> Maybe Placement
    searchCond acc (pos, x)
      | take len x == word = Just (adjustedPos pos, orient)
      | otherwise = acc

    searchGrid :: [(Posn, String)]
    searchGrid = zip (map fst grid) (tails $ map snd grid)

    adjustedPos :: Posn -> Posn
    adjustedPos (x,y) = (x-1, y-1)

-- | Creation of Grid with Position for various orientations
searchingTypes :: WordSearchGrid -> [(GridWithPos, Orientation)]
searchingTypes grid = zip [gridWithPos,
                       reverse gridWithPos,
                       concat $ transpose twoDGridWithPos,
                       reverse $ concat $ transpose twoDGridWithPos,
                       diagonalString twoDGridWithPos,
                       diagonalString $ transpose twoDGridWithPos,
                       diagonalString $ reverse twoDGridWithPos,
                       diagonalString $ transpose $ reverse twoDGridWithPos] ordOrient
  where
    gridWithPos :: GridWithPos
    gridWithPos = zip indices (concat grid)

    indices :: [Posn]
    indices = [(y,x) | x <- [0 .. gridLen - 1], y <- [0 .. gridLen - 1]]

    gridLen = length grid

    twoDGridWithPos :: [[(Posn, Char)]]
    twoDGridWithPos = zipWith (zip . twoDIndices) [0 ..] grid

    twoDIndices :: Int -> [Posn]
    twoDIndices x = [(y,x) | y <- [0 .. gridLen - 1]]

-- | Order of orientation used for zipping
ordOrient :: [Orientation]
ordOrient = [Forward, Back, Down, Up, UpForward, DownBack, DownForward, UpBack]

-- | Main function to solve word search
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch words grid = map (\word -> ( word,
                                             foldl
                                             (helpSolveWordSearch word) Nothing (searchingTypes $ borderedGrid grid))) words
  where

    helpSolveWordSearch ::  String -> Maybe Placement -> (GridWithPos, Orientation) -> Maybe Placement
    helpSolveWordSearch word placement searchType@(grid, orient)
      | isJust foundType = search word searchType
      | otherwise = placement
      where
        foundType :: Maybe Placement
        foundType = search word searchType
    gridLen = length grid

-- | Create a bordered grid for when converted to string then continuous -'s aren't used
borderedGrid :: WordSearchGrid -> WordSearchGrid
borderedGrid grid = verticalBorder ++ map (\str -> "-" ++ str ++ "-") grid ++ verticalBorder
  where
    gridLen = length grid

    verticalBorder :: [[Char]]
    verticalBorder = [replicate (gridLen + 2) '-']

-- | Create unbordered grid from bordered grid
unborderedGrid :: WordSearchGrid -> WordSearchGrid
unborderedGrid grid = map (init . tail) (init $ tail grid)

-- Two examples for you to try out, the first of which is in the instructions

testDiag1'1 = "HSAGAGMCCNEGSAITKHTGRINNKAETMRILMCTSGAAREIKAHIMATTLEKZRHLGKLKOCECIDSLTNDASENGRRDG"

-- Challenge 2 --

-- | Find list of positions given the starting position, word and it's orientation
findPos :: Posn -> Orientation -> String -> [Posn]
findPos (x,y) orient word = zip x_r y_r
  where

    len = length word

    x_s = replicate len x
    y_s = replicate len y
    forward = [x .. (x + len - 1)]
    back = [x, x-1 .. (x - len + 1)]
    down = [y .. (y + len - 1)]
    up = [y, y-1 .. (y - len - 1)]

    (x_r, y_r, _) = posRange

    posRange :: ([Int], [Int], Orientation)
    posRange = head $ dropWhile (\(x_r, y_r, otype) -> orient /= otype) posList

    posList :: [([Int],[Int], Orientation)]
    posList = zip3 [forward, back, x_s, x_s, forward, back, forward, back]
                   [y_s, y_s, down, up, up, down, down, up]
                   ordOrient


-- | Check how many instances of word are present in the grid
searchInstances :: [String] -> String -> Int
searchInstances grids word = sum $ map helpSearch grids
  where
    len = length word

    helpSearch :: String -> Int
    helpSearch grid = foldl (\acc x -> if take len x == word then acc + 1 else acc) 0 (tails grid)

-- | Created and empty grid of a particular density
createGrid :: Int -> Double -> WordSearchGrid
createGrid maxLen density = replicate sideLen $ replicate sideLen '_'
  where
    sideLen :: Int
    sideLen
      | density < 1 = ceiling (1.0 / density) * maxLen
      | otherwise = ceiling (1.0 / (density - 0.01)) * maxLen

-- | Fill Words in random positions and orientations in empty grid
helpFillWord :: WordSearchGrid -> String -> IO WordSearchGrid
helpFillWord currGrid word = do
  (randomOrientIndex, seed2) <- fmap (randomR (0,7)) newStdGen

  let currOrient = ordOrient !! randomOrientIndex
  let strOrient  = fst $ strGridsWithPos currGrid !! randomOrientIndex

  let (xcoord, seed3) = randomR (1, gridLen - 2) seed2 :: (Int, StdGen)
  let (ycoord, _) = randomR (1, gridLen - 2) seed3 :: (Int, StdGen)

  let wordToBeChecked = take (length word) $ dropWhile (\(pos,chr) -> pos /= (xcoord, ycoord)) strOrient

  let acceptableWord = all (\(a,b) -> a == b || b == '_') (zip word $ map snd wordToBeChecked)
  let newGrid = foldl replace currGrid $ zip (findPos (xcoord,ycoord) currOrient word) word
  if acceptableWord then
    return newGrid
  else
    helpFillWord currGrid word

  where
    gridLen = length currGrid

    replace :: WordSearchGrid -> (Posn,Char) -> WordSearchGrid
    replace grid ((x',y'), letter) = do
       (x,row) <- zip [0..] grid
       return [if x == x' && y == y' then letter else r | (y,r) <- zip [0..] row]

-- | Make a list of strings of words in orientation
strGrids :: WordSearchGrid -> [String]
strGrids = map (map snd . fst) . strGridsWithPos

-- | Make a list of strings of words in orientation and position
strGridsWithPos :: WordSearchGrid -> [(GridWithPos, Orientation)]
strGridsWithPos = searchingTypes . borderedGrid

-- | Main function for creating word search
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch wordList density = do
  -- create empty grid
  let emptyGrid = createGrid (length $ maximumBy (Data.Ord.comparing length) wordList) density
  -- A list of unique letters to fill in word search list consisting of letters from the given words
  let uniqueLetters = nub $ concat wordList
  -- Fill with words and then random letters
  finalWordFill <- foldM helpFillWord emptyGrid wordList
  finalGrid <- mapM (mapM $ finalRandFill uniqueLetters) finalWordFill
  if correctGrid finalGrid wordList then
      return finalGrid
  else
  -- Start again
      createWordSearch wordList density

-- | After filling the grid there should be only one string instance in any orientation
correctGrid :: WordSearchGrid -> [String] -> Bool
correctGrid grid wordList = all (==1) strInstances
  where
    strInstances :: [Int]
    strInstances = map (searchInstances $ strGrids grid) wordList

-- | After the grid has been filled with words fill other positions with random characters
finalRandFill :: String -> Char -> IO Char
finalRandFill letterRange x = do
  (randIdx, _) <- fmap (randomR (0, length letterRange - 1)) newStdGen
  if x == '_' then
    return (letterRange !! randIdx)
  else
    return x

--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws

-- | Print grid with borders
printGrid' :: WordSearchGrid -> IO ()
printGrid' grid = printGrid $ borderedGrid grid

-- Challenge 3 --

-- | prettyPrint LamVar
printVar :: Int -> String
printVar i = 'x':show i

-- | prettyPrint LamAbsVar
printAbs :: Int -> String
printAbs i = "\\" ++ printVar i ++ "->"

-- | Given an expression and macros return pretty printed string
abstractExpr :: LamExpr -> [(String,LamExpr)] -> String
abstractExpr expr macros = case expr of
                      LamVar x -> printVar x
                      LamAbs x remExpr -> printAbs x ++ abstractExpr remExpr macros
                      LamMacro str -> str
                      LamApp expr1 expr2@(LamApp _ _) -> printExpr expr1  ++ "(" ++ abstractExpr expr2 macros ++ ")"
                      LamApp expr1@(LamAbs _ _) expr2 -> printBracketedExpr expr1 ++ " " ++ abstractExpr expr2 macros
                      LamApp expr1 expr2 -> printExpr expr1 ++ " " ++ printExpr expr2
  where
    printBracketedExpr :: LamExpr -> String
    printBracketedExpr remExpr
      | isNothing $ advLookUpExpr remExpr = "(" ++ printExpr remExpr ++ ")"
--      Note that after testing if less testcases replace below with above
--      | isNothing $ lookUpExpr remExpr = "(" ++ printedExpr ++ ")"
      | otherwise = printExpr remExpr

    printExpr :: LamExpr -> String
    printExpr remExpr
     | isNothing $ advLookUpExpr remExpr  = abstractExpr remExpr macros
     | otherwise = fst $ fromMaybe ("Illegal Macro", LamVar (-1)) $ advLookUpExpr remExpr
--      Note that after testing if less testcases replace below 2 lines with above 2 lines
--      | isNothing $ lookUpExpr remExpr  = abstractExpr remExpr macros
--      | otherwise = fromMaybe "Illegal Macro" $ lookUpExpr remExpr

    lookUpExpr :: LamExpr -> Maybe String
    lookUpExpr remExpr = lookup remExpr (map swap macros)

    advLookUpExpr :: LamExpr -> Maybe (String, LamExpr)
    advLookUpExpr exp1 = find (\(str,exp2) -> eqSyntaxExpr [] exp1 exp2) macros

-- | Checks whether 2 expressions are syntactically equal
eqSyntaxExpr :: [(Int, Int)] -> LamExpr -> LamExpr -> Bool
eqSyntaxExpr varLookUp expr1 expr2 = case (expr1,expr2) of
                                       (LamVar x,LamVar y) -> checkEq x y
                                       (LamApp e1 e2, LamApp e3 e4) -> eqSyntaxExpr varLookUp e1 e3 && eqSyntaxExpr varLookUp e2 e4
                                       -- If there exists another variable with same i1 nub's functionality will remove it
                                       -- So scoping is maintained
                                       (LamAbs i1 e1, LamAbs i2 e2) -> eqSyntaxExpr  (nub $ (i1,i2):varLookUp) e1 e2
                                       (LamMacro s1, LamMacro s2) -> s1 == s2
                                       _ -> False
   where
     -- Either a variable on lookup equal or a free variable
     checkEq :: Int -> Int -> Bool
     checkEq a b = lookUpInt a b  || (isNothing (lookup a varLookUp) && lookUpInt a b)

     lookUpInt :: Int -> Int -> Bool
     lookUpInt a b = fromMaybe (-1) (lookup a varLookUp) == b

-- | Main function for pretty printing
prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef defs expr) = concatMap helpPrintDef defs ++ abstractExpr expr defs
  where
    helpPrintDef :: (String, LamExpr) -> String
    helpPrintDef (macro, expr) = "def " ++ macro ++ "=" ++ abstractExpr expr [] ++ " in "

-- Challenge 4 --

-- data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
-- data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
--                LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

parseMacroExpr :: Parser LamMacroExpr
parseMacroExpr = do
  def <- parseDef
  (LamDef defs expr) <- parseMacroExpr
  return (LamDef (def:defs) expr)
  <|> do
  expr <- parseExpr
  return (LamDef [] expr)

-- parseDefs :: [(String, LamExpr)] -> Parser [(String, LamExpr)]
-- parseDefs defs = do
--   newDef <- parseDef
--   do
--     finalDef <- parseDefs (newDef:defs)
--     return (newDef:finalDef)
--     <|> return (newDef:defs)
--   <|> return []

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
parseExpr = parseBrac <|> parseVar <|> parseMacro <|> parseAbs

parseVar :: Parser LamExpr
parseVar = do
  char 'x'
  var <- nat
  parseApp (LamVar var)

parseMacro :: Parser LamExpr
parseMacro = do
  x <- some upper
  parseApp (LamMacro x)

parseApp :: LamExpr -> Parser LamExpr
parseApp expr1 = do
  space
  expr2 <- parseExpr
  return (LamApp expr1 expr2)
  <|> return expr1

parseAbs :: Parser LamExpr
parseAbs = do
  symbol "\\"
  lamVar <- parseVar
  let (LamVar var) = lamVar
  symbol "->"
  expr <- parseExpr
  parseApp (LamAbs var expr)

parseBrac :: Parser LamExpr
parseBrac = do
  char '('
  expr <- parseExpr
  char ')'
  parseApp expr

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro str = case parse parseMacroExpr str of
                      [] -> Nothing
                      (x,""):_ -> if checkMacroExpr x then Just x else Nothing
                      _ -> Nothing

checkMacroExpr :: LamMacroExpr -> Bool
checkMacroExpr (LamDef defs expr) = nub macroStrList == macroStrList && macroCheckScope
  where
    macroStrList :: [String]
    macroStrList = map fst defs

    macroCheckScope :: Bool
    macroCheckScope = all (checkScope [] . snd) defs

checkScope :: [Int] -> LamExpr -> Bool
checkScope xs expr = case expr of
                    (LamVar x) -> x `elem` xs
                    (LamMacro _) -> True
                    (LamAbs i remExpr) -> checkScope (i:xs) remExpr
                    (LamApp expr1 expr2) -> checkScope xs expr1 && checkScope xs expr2

usedVarAbs :: LamExpr -> [Int]
usedVarAbs mainExpr = nub $ helpUsedVar mainExpr
  where
    helpUsedVar :: LamExpr -> [Int]
    helpUsedVar lamExpr = case lamExpr of
                         (LamVar x) -> [x]
                         (LamMacro _) -> []
                         (LamAbs i remExpr) -> i:helpUsedVar remExpr
                         (LamApp expr1 expr2) -> helpUsedVar expr1 ++ helpUsedVar expr2

-- Challenge 5

-- | Take maxmimum of used variables and generate a new variable
newVarGen :: [Int] -> Int
newVarGen a = maximum a + 1

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
helpTransform n (LamApp expr1 expr2) = LamAbs n (LamApp (helpTransform (n+1) expr1)
                                                  (LamAbs (n+2) (LamApp (helpTransform (n+3) expr2)
                                                                 (LamAbs (n+4) (LamApp
                                                                                (LamApp (LamVar (n+2))
                                                                                 (LamVar (n+4)))
                                                                                 (LamVar n))))))
helpTransform _ (LamMacro x) = LamMacro x
-- Examples in the instructions
sfdsf = LamDef [] (LamAbs 3 (LamApp
                             (LamAbs 4 (LamApp (LamVar 4) (LamVar 1)))
                             (LamAbs 5 (LamApp
                                        (LamAbs 6 (LamApp (LamVar 6) (LamVar 2)))
                                        (LamAbs 7 (LamApp (LamApp
                                                           (LamVar 5)
                                                           (LamVar 7))
                                                   (LamVar 3)))))))
-- Challenge 6

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = nub $ xs ++ ys


rename :: Int -> Int -> LamExpr -> LamExpr
rename x y (LamVar z)
  | z == x = LamVar y
  | otherwise = LamVar z
rename x y (LamAbs z m)
  | z == x = LamAbs z m
  | otherwise = LamAbs z (rename x y m)
rename x y (LamApp m n) = LamApp (rename x y m) (rename x y n)
rename _ _ lamMacro = lamMacro

substituteVar :: Int -> LamExpr -> LamExpr -> LamExpr
substituteVar x n (LamVar y)
  | y == x = n
  | otherwise = LamVar y
substituteVar x n (LamAbs y m)
  | y == x = LamAbs y m
  | otherwise = LamAbs genZ (substituteVar x n $ rename y genZ m) ----
  where
    genZ :: Int
    genZ = checkZ $ merge (usedVarAbs m) (usedVarAbs n)

    checkZ :: [Int] -> Int
    checkZ list
      | newZ list == x = newZ (x:list)
      | otherwise = newZ list
    newZ :: [Int] -> Int

    newZ list = newVarGen list
substituteVar x n (LamApp m1 m2) = LamApp (substituteVar x n m1) (substituteVar x n m2)
substituteVar _ _ lamMacro = lamMacro

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
                                       [substituteVar z m n] ++
                                       [LamApp (LamAbs z n) zs | zs <- checkMacro macros m]

helpInner macros (LamApp n m) =
                            [LamApp xs m | xs <- helpInner macros n] ++
                            [LamApp n ys | ys <- helpInner macros m]

helpInner macros (LamAbs n m) = [LamAbs n xs | xs <- helpInner macros m]
helpInner macros expr@(LamMacro m) = checkMacro macros expr
helpInner macros _ = []

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

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef macros expr)
  | null reducedExp = Nothing
  | otherwise = Just (LamDef macros (head reducedExp))
  where
    reducedExp = helpOuter macros expr

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

exId =  LamAbs 1 (LamVar 1)
-- (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)
wExp = LamAbs 1 (LamApp (LamVar 1) (LamVar 1))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4)
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp exId (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") )

ex6'8 = LamDef [("G", LamAbs 1 (LamAbs 2 (LamVar 1))),
               ("F", LamAbs 1 (LamAbs 2 (LamVar 2)))]
        (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamMacro "G")
                                            (LamApp
                                                   (LamApp (LamMacro "F") (LamVar 2))
                                                   (LamVar 1)))
                                    (LamVar 2))))

eeeee = LamDef [("G",LamAbs 1 (LamAbs 2 (LamVar 1))),
                ("F",LamAbs 1 (LamAbs 2 (LamVar 2)))]
        (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1)))
                                            (LamApp   (LamApp (LamMacro "F") (LamVar 2))
                                                      (LamVar 1)))
                                    (LamVar 2))))


stepPrintInner n expr = prettyPrint . fromJust . last . take n $ iterate (maybe Nothing innerRedn1) (Just expr)
stepInner n expr =  fromJust . last . take n $ iterate (maybe Nothing innerRedn1) (Just expr)
stepPrintOuter n expr = prettyPrint . fromJust . last . take n $ iterate (maybe Nothing outerRedn1) (Just expr)
stepOuter n expr =  fromJust . last . take n $ iterate (maybe Nothing outerRedn1) (Just expr)
stepOuterIt n expr =  take n $ iterate (maybe Nothing outerRedn1) (Just expr)
stepInnerIt n expr =  takeWhile isJust $ iterate (maybe Nothing innerRedn1) (Just expr)
