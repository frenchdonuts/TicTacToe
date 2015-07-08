module Main (
  main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Utils

import ModelsTest
import GameLogicTest
import Presentation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "TicTacToe Tests" [testModels]


testModels :: TestTree
testModels =
  testGroup "Models" [testBoardOperations]

testBoardOperations = testGroup "Board Operations"
  [
    testProperty "Out of bounds Board get" propOutOfBoundsGetIsNothing,
    testProperty "Out of bounds Board set" propOutOfBoundsSetIsNothing,
    testProperty "Set/Get monadic composition" propSetThenGetPlayerEqual,
    testProperty "Get/Set composition" propGetThenSetPlayerEqual
  ]


testGameLogic :: TestTree
testGameLogic = testGroup "Game Logic"
  [
    testCase     "Empty board is not full"    assertEmptyBoardIsNotFull,
    testProperty "0 Nothings in a full board" propNoNothingsInFullBoard,
    testCase     "X Wins state"               testFindWinner1,
    testCase     "Tie state"                  testFindWinner2
  ]
