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
diagonalString :: [GridWithPos] -> GridWithPos
diagonalString wds = [ wds !! (x - y) !! y | x <- [0..(len - 1)] , y <- [0 .. x]] ++
                     [ wds !! y !! (len + x - y) | x <- [0..len] , y <- [len, (len - 1) .. x]]
  where
    len = length wds - 1

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

ordOrient :: [Orientation]
ordOrient = [Forward, Back, Down, Up, UpForward, DownBack, DownForward, UpBack]

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

borderedGrid :: WordSearchGrid -> WordSearchGrid
borderedGrid grid = verticalBorder ++ map (\str -> "-" ++ str ++ "-") grid ++ verticalBorder
  where
    gridLen = length grid

    verticalBorder :: [[Char]]
    verticalBorder = [replicate (gridLen + 2) '-']

unborderedGrid :: WordSearchGrid -> WordSearchGrid
unborderedGrid grid = map (init . tail) (init $ tail grid)

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

-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]

testDiag1'1 = "HSAGAGMCCNEGSAITKHTGRINNKAETMRILMCTSGAAREIKAHIMATTLEKZRHLGKLKOCECIDSLTNDASENGRRDG"

-- Challenge 2 --

searchInstances :: [String] -> String -> Int
searchInstances grids word = sum $ map helpSearch grids
  where
    len = length word

    helpSearch :: String -> Int
    helpSearch grid = foldl (\acc x -> if take len x == word then acc + 1 else acc) 0 (tails grid)

createGrid :: Int -> Double -> WordSearchGrid
createGrid maxLen density = replicate sideLen $ replicate sideLen '_'
  where
    sideLen :: Int
    sideLen = ceiling (1.0 / (density - 0.01)) * maxLen

helpFillWord :: WordSearchGrid -> String -> IO WordSearchGrid
helpFillWord currGrid word = do
  seed <- newStdGen
  let (randomOrientIndex, seed2) = randomR (0,7) seed :: (Int, StdGen)
  let currOrient = ordOrient !! randomOrientIndex
  let strOrient  = fst $ strGridsWithPos currGrid !! randomOrientIndex

  let (xcoord, seed3) = randomR (1, gridLen - 2) seed2 :: (Int, StdGen)
  let (ycoord, _) = randomR (1, gridLen - 2) seed3 :: (Int, StdGen)

  let wordToBeChecked = take (length word) $ dropWhile (\(pos,chr) -> pos /= (xcoord, ycoord)) strOrient

  let acceptableWord = all (\(a,b) -> a == b || b == '_') (zip word $ map snd wordToBeChecked)
  let newGrid = foldl replace currGrid (zip (findPos (xcoord,ycoord) currOrient word) word)
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

strGrids :: WordSearchGrid -> [String]
strGrids grid = map (map snd . fst) (strGridsWithPos grid)

strGridsWithPos :: WordSearchGrid -> [(GridWithPos, Orientation)]
strGridsWithPos grid = searchingTypes (borderedGrid grid)

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch wordList density = do
  let emptyGrid = createGrid (length $ maximumBy (Data.Ord.comparing length) wordList) density
  let uniqueLetters = nub $ concat wordList
  finalWordFill <- foldM helpFillWord emptyGrid wordList
  finalGrid <- mapM (mapM $ finalRandFill uniqueLetters) finalWordFill
  let finalStrGrids = strGrids finalGrid
  let strInstances = map (searchInstances finalStrGrids) wordList
  let correctGrid = all (==1) strInstances
  if correctGrid then
      return finalGrid
  else
    createWordSearch wordList density

finalRandFill :: String -> Char -> IO Char
finalRandFill letterRange x = do
  seed <- newStdGen
  let (randIdx, _) = randomR (0, length letterRange - 1) seed :: (Int, StdGen)
  if x == '_' then
    return (letterRange !! randIdx)
  else
    return x

--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid' g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws

printGrid' :: WordSearchGrid -> IO ()
printGrid' grid = printGrid $ borderedGrid grid

-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint _ = ""

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))


-- Challenge 4 --

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro _ = Nothing


-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
exId =  (LamAbs 1 (LamVar 1))
ex5'1 = (LamApp (LamVar 1) (LamVar 2))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

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
