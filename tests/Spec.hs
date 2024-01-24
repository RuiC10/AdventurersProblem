module Main where

import Test.HUnit 
import Adventurers

testLeq17 :: Test
testLeq17 = "There is a solution when limited to 5 moves and 17 seconds" ~: True ~=? leq17' 

testL17 :: Test
testL17 = "There is no solution when limited to 5 moves and less then 17 seconds" ~: False ~=? l17'

tests = test [testLeq17, testL17]

main :: IO ()
main = runTestTTAndExit tests
