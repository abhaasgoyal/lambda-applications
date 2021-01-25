{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

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

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]

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
  let strInstances = map (searchInstances $ strGrids finalGrid) wordList
  -- After filling the grid there should be only one string instance
  let correctGrid = all (==1) strInstances
  if correctGrid then
      return finalGrid
  else
  -- Start again
      createWordSearch wordList density

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
printAbs i = "/" ++ printVar i ++ "->"

-- | Given an expression and macros return pretty printed string
abstractExpr :: LamExpr -> [(String,LamExpr)] -> String
abstractExpr expr macros = case expr of
                      LamVar x -> printVar x
                      LamAbs x remExpr -> printAbs x ++ " " ++ abstractExpr remExpr macros
                      LamMacro str -> str
                      LamApp expr1@(LamAbs _ _) expr2 -> printBracketedExpr expr1 ++ " " ++ abstractExpr expr2 macros
                      LamApp expr1 expr2@(LamApp _ _) -> printExpr expr1  ++ "(" ++ abstractExpr expr2 macros ++ ")"
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
                                       (LamAbs i1 e1, LamAbs i2 e2) -> checkEq i1 i2 || eqSyntaxExpr  (nub $ (i1,i2):varLookUp) e1 e2
                                       (LamMacro s1, LamMacro s2) -> s1 == s2
                                       _ -> False
   where
     checkEq :: Int -> Int -> Bool
     checkEq a b = a == b || lookUpInt a b

     lookUpInt :: Int -> Int -> Bool
     lookUpInt a b = fromMaybe (-1) (lookup a varLookUp) == b

-- | Main function for pretty printing
prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef defs expr) = concatMap helpPrintDef defs ++ abstractExpr expr defs
  where
    helpPrintDef :: (String, LamExpr) -> String
    helpPrintDef (macro, expr) = "def " ++ macro ++ "=" ++ abstractExpr expr [] ++ " in "

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))
ex3'5 = LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 3) (LamVar 4)))

-- Challenge 4 --

-- data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
-- data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
--                LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

parseMacroExpr :: Parser LamMacroExpr
parseMacroExpr = do
  defs <- parseDefs []
  expr <- parseExpr
  return (LamDef defs expr)

parseDefs :: [(String, LamExpr)] -> Parser [(String, LamExpr)]
parseDefs defs = do
  newDef <- parseDef
  if null newDef then
    return defs
  else do
    finalDef <- parseDefs (newDef:defs)
    return (newDef:finalDef)
    <|> return (newDef:defs)

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
  space
  string "in"
  minSpace
  return (x, expr)

parseExpr :: Parser LamExpr
parseExpr = parseApp <|> parseExprWOApp

parseExprWOApp :: Parser LamExpr
parseExprWOApp = parseBrac <|> parseVar <|> parseMacro <|> parseAbs

parseVar :: Parser LamExpr
parseVar = do
  char 'x'
  var <- natural
  return (LamVar var)

parseMacro :: Parser LamExpr
parseMacro = do
  x <- some upper
  return (LamMacro x)

parseApp :: Parser LamExpr
parseApp = do
  expr1 <- parseExprWOApp
  minSpace
  expr2 <- parseExprWOApp
  return (LamApp expr1 expr2)

parseAbs :: Parser LamExpr
parseAbs = do
  symbol "\\"
  lamVar <- parseVar
  let (LamVar var) = lamVar
  symbol "->"
  expr <- parseExpr
  return (LamAbs var expr)

parseBrac :: Parser LamExpr
parseBrac = do
  char '('
  expr <- parseExpr
  char ')'
  return expr

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro str = case parse parseMacroExpr str of
                      [] -> Nothing
                      (x,str):_ -> Just x

-- Challenge 5

-- | Take maxmimum of used variables and generate a new variable
newVarGen :: [Int] -> Int
newVarGen a = maximum a + 1

-- | List of usedVariables
listUsedVar :: LamMacroExpr -> [Int]
listUsedVar (LamDef defs expr) = nub $ concatMap (helpList . snd) defs ++ helpList expr
  where
    helpList :: LamExpr -> [Int]
    helpList (LamVar x) = [x]
    helpList (LamMacro _) = []
    helpList (LamAbs i expr) = i:helpList expr
    helpList (LamApp expr1 expr2) = helpList expr1 ++ helpList expr2

-- | Main function for transforming in cps
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform lam@(LamDef defs expr) = LamDef (zip (map fst defs) (map (helpTransform newVar . snd) defs)) (helpTransform newVar expr)
  where
    newVar :: Int
    newVar = newVarGen $ listUsedVar lam

-- | Given a newVariable and an expression convert to cps
helpTransform :: Int -> LamExpr -> LamExpr
helpTransform n (LamVar x) = LamAbs n (LamApp (LamVar n) (LamVar x))
helpTransform n (LamAbs i expr) = LamAbs n (LamApp (LamVar n) (LamAbs i (helpTransform (n+1) expr)))
helpTransform n (LamApp expr1 expr2) = LamAbs n (LamApp (helpTransform (n+1) expr1)
                                                  (LamAbs (n+2) (LamApp (helpTransform (n+3) expr2)
                                                                 (LamAbs (n+4) (LamApp
                                                                                (LamApp (LamVar (n+2))
                                                                                 (LamVar (n+4)))
                                                                                 (LamVar n))))))
helpTransform _ (LamMacro x) = LamMacro x
-- Examples in the instructions
exId =  LamAbs 1 (LamVar 1)
ex5'1 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
ex5'2 = LamDef [ ("F", exId) ] (LamVar 2)
ex5'3 = LamDef [ ("F", exId) ] (LamMacro "F")
ex5'4 = LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F"))
sfdsf = LamDef [] (LamAbs 3 (LamApp
                             (LamAbs 4 (LamApp (LamVar 4) (LamVar 1)))
                             (LamAbs 5 (LamApp
                                        (LamAbs 6 (LamApp (LamVar 6) (LamVar 2)))
                                        (LamAbs 7 (LamApp (LamApp
                                                           (LamVar 5)
                                                           (LamVar 7))
                                                   (LamVar 3)))))))
-- Challenge 6

-- | Inner reduction helper
helpInnerRed :: LamExpr -> LamExpr
helpInnerRed expr = case expr of
                        (LamApp (LamAbs i expr1) expr2) -> helpInnerRed $ replaceVar (checkRev i expr2) expr1
                        (LamAbs i remExpr) -> LamAbs i (helpInnerRed remExpr)
                        _ -> expr

innerRedn1 :: LamMacroExpr -> Int -> Maybe LamMacroExpr
innerRedn1 _ _ = Nothing

-- | Outer reduction helper
helpOuterRed :: LamExpr -> LamExpr
helpOuterRed expr = case expr of
                        (LamApp (LamAbs i expr1) expr2) -> replaceVar (checkRev i expr2) expr1
                        (LamAbs i remExpr) -> LamAbs i (helpOuterRed remExpr)
                        _ -> expr

replaceVar :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
replaceVar repFunc oldExpr = case oldExpr of
                              (LamApp expr1 expr2) -> LamApp (replaceVar repFunc expr1) (replaceVar repFunc expr2)
                              (LamAbs i expr) -> LamAbs i (replaceVar repFunc expr)
                              _ -> repFunc oldExpr

checkRev :: Int -> LamExpr -> LamExpr -> LamExpr
checkRev oldVar newExpr (LamVar v) = if v == oldVar then newExpr else LamVar v
checkRev _ _ oldExpr = oldExpr

checkMacro :: String -> LamExpr -> LamExpr -> LamExpr
checkMacro oldMac newExpr (LamMacro m) = if m == oldMac then newExpr else LamMacro m
checkMacro _ _ oldExpr = oldExpr

outerRedn1 :: LamMacroExpr -> Int -> Maybe LamMacroExpr
outerRedn1 _ _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing)

-- Examples in the instructions

-- (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4)
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") )
