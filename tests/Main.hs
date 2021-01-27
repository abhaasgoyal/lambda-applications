-- | Test Runner

module Main where

import Tests


main :: IO ()
main = do
  runTests test1
  testCreatedGrid "Test Creation1" exWords1'1 0.3
  runTests test2
  exitSuccess


