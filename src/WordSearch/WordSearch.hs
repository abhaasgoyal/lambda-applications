module WordSearch where
import Types
import qualified Data.Ord
import Data.List
import System.Random
import Control.Monad
import Data.Maybe

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
    helpSolveWordSearch word placement searchType
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
  -- Choose Random orientation
  (randomOrientIndex, seed2) <- fmap (randomR (0,7)) newStdGen
  let currOrient = ordOrient !! randomOrientIndex
  -- Generate string traversing in the respective orientatoin
  let strOrient  = fst $ strGridsWithPos currGrid !! randomOrientIndex
  -- Choose Random point
  let [xcoord, ycoord] = take 2 $ randomRs (1,gridLen-2) seed2
  -- Checking eligibility of the word
  let wordToBeChecked = take (length word) $ dropWhile (\(pos,chr) -> pos /= (xcoord, ycoord)) strOrient
  let acceptableWord = all (\(a,b) -> a == b || b == '_') (zip word $ map snd wordToBeChecked)
  -- Filling new grid in case correct grid is formed
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

-- | After the grid has been filled only with words fill other positions with random characters
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
