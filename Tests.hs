-- | List of tests

module Tests where

import Challenges
import System.Exit
import Data.Maybe

data Test = Test String TestResult

data TestResult = PASS | Fail String deriving (Eq, Show)

assertEq :: (Eq a, Show a) => a -> a -> TestResult
assertEq actual expected
  | actual == expected = PASS
  | otherwise = Fail (show actual ++ " is not equal to\n" ++ show expected)

testList :: [TestResult] -> TestResult
testList t = case mapMaybe failMessage t of
  []       -> PASS
  messages -> Fail (unlines messages)
  where
    failMessage :: TestResult -> Maybe String
    failMessage PASS       = Nothing
    failMessage (Fail m) = Just m

runTests :: [Test] -> IO ()
runTests t = case t of
  [] -> return()
  Test msg PASS:ts -> do
    putStrLn ("PASS: " ++ msg)
    runTests ts
  Test msg (Fail failMsg):_ -> do
    putStrLn ("FAIL: " ++ msg)
    putStrLn failMsg
    exitFailure

main :: IO ()
main = do
  runTests test1
  testCreatedGrid "Test Creation1" exWords1'1 0.3
  runTests test2
  exitSuccess

test1 :: [Test]
test1 = [testSolveWordSearch]

testCreatedGrid :: String -> [String] -> Float -> IO ()
testCreatedGrid msg words density = do
  newWordSearch <- createWordSearch exWords1'1 0.3
  let correctedGrid = correctGrid newWordSearch words
  let correctedSolve = all ((/=Nothing) . snd) $ solveWordSearch words newWordSearch
  runTests [Test msg (assertEq correctedGrid True), Test msg (assertEq correctedSolve True)]

testSolveWordSearch :: Test
testSolveWordSearch = Test
                "Solve Word Search"
                (testList [
                    assertEq
                    (solveWordSearch exWords1'1 exGrid1'1)
                    [("HASKELL",Just ((0,0),DownForward)),
                     ("STRING",Just ((7,0),Back)),
                     ("STACK",Just ((2,2),Forward)),
                     ("MAIN",Just ((2,7),Up)),
                     ("METHOD",Just ((4,3),Down))],
                    assertEq
                    (solveWordSearch exWords1'2 exGrid1'2)
                    [("BANANA",Just ((5,6),UpBack)),
                     ("ORANGE",Just ((1,0),DownForward)),
                     ("MELON",Just ((7,8),Up)),
                     ("RASPBERRY",Just ((8,0),DownBack)),
                     ("APPLE",Just ((2,8),UpForward)),
                     ("PLUM",Just ((5,1),DownBack)),
                     ("GRAPE",Just ((8,6),Up))]
                          ])


test2 :: [Test]
test2 = [testPrettyPrint, testParse, testCPS, testRed]

testPrettyPrint :: Test
testPrettyPrint = Test
                   "Pretty Printing"
                (testList [
                    assertEq
                    (prettyPrint ex3'1) "(\\x1->x1) \\x1->x1",
                    assertEq
                    (prettyPrint ex3'2) "\\x1->x1 \\x1->x1",
                    assertEq
                    (prettyPrint ex3'3) "def F=\\x1->x1 in \\x2->x2 F",
                    assertEq
                    (prettyPrint ex3'4) "def F=\\x1->x1 in \\x2->F x2"
                          ])

testParse :: Test
testParse = Test
            "Parsing"
            (testList[
                assertEq
                (parseLamMacro "def F=\\x1->x1 in F") (Just (LamDef [("F",LamAbs 1 (LamVar 1))] (LamMacro "F"))),
                assertEq
                (parseLamMacro "(x1)") (Just (LamDef [] (LamVar 1))),
                assertEq
                (parseLamMacro "invalid") Nothing,
                assertEq
                (parseLamMacro "(\\x1 -> x1 x2)") (Just ex6'1),
                assertEq
                (parseLamMacro "def F=\\x1 -> x1 in F") (Just ex6'2),
                assertEq
                (parseLamMacro "(\\x1->x1)(\\x2->x2)") (Just ex6'3),
                assertEq
                (parseLamMacro "(\\x1->x1 x1)(\\x1->x1 x1)") (Just ex6'4),
                assertEq
                (parseLamMacro "def ID=\\x1 -> x1 in def FST=(\\x1->\\x2->x1) in FST x3 (ID x4)") (Just ex6'5),
                assertEq
                (parseLamMacro "def FST=(\\x1->\\x2->x1) in FST x3 ((\\x1 -> x1) x4)") (Just ex6'6),
                assertEq
                (parseLamMacro "def ID=\\x1 -> x1 in def SND=\\x1->\\x2->x2 in SND((\\x1->x1 x1) \\x1->x1 x1) ID") (Just ex6'7)

                     ])

testCPS :: Test
testCPS = Test
          "CPS Transform"
          (testList[
              assertEq (prettyPrint $ cpsTransform ex5'1)
              "\\x3->(\\x4->x4 x1) \\x5->(\\x6->x6 x2) \\x7->x5 x7 x3",
              assertEq (prettyPrint $ cpsTransform ex5'2)
              "def F=\\x3->x3 \\x1->\\x4->x4 x1 in \\x3->x3 x2",
              assertEq (prettyPrint $ cpsTransform ex5'3)
              "def F=\\x2->x2 \\x1->\\x3->x3 x1 in F",
              assertEq (prettyPrint $ cpsTransform ex5'4)
              "def F=\\x2->x2 \\x1->\\x3->x3 x1 in \\x2->F \\x4->F \\x6->x4 x6 x2"
                   ])

testRed :: Test
testRed = Test
          "Reduction Strategies"
          (testList [
              assertEq (compareInnerOuter ex6'1 10) (Just 0, Just 0, Just 6, Just 6),
              assertEq (compareInnerOuter ex6'1 6) (Just 0, Just 0, Just 6, Just 6),
              assertEq (compareInnerOuter ex6'2 10) (Just 1, Just 1, Just 3, Just 3),
              assertEq (compareInnerOuter ex6'3 10) (Just 1, Just 1, Just 8, Just 8),
              assertEq (compareInnerOuter ex6'4 100) (Nothing, Nothing, Nothing, Nothing),
              assertEq (compareInnerOuter ex6'5 30) (Just 5, Just 3, Just 22, Just 22),
              -- Given wrong in pdf
              assertEq (compareInnerOuter ex6'6 30) (Just 4, Just 3, Just 21, Just 21),
              assertEq (compareInnerOuter ex6'7 1000) (Nothing, Just 4, Nothing, Nothing)
                    ])
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]

ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))
ex3'5 = LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 3) (LamVar 4)))

exId =  LamAbs 1 (LamVar 1)
ex5'1 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
ex5'2 = LamDef [ ("F", exId) ] (LamVar 2)
ex5'3 = LamDef [ ("F", exId) ] (LamMacro "F")
ex5'4 = LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F"))

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
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") )
