-- | Main module for testing
module Main where

import System.Exit
import Tests

main :: IO ()
main = do
  runTests test1
  testCreatedGrid "Test Creation1" exWords1'1 0.3
  runTests test2
  exitSuccess
