module GameLogicTest where


import Test.HUnit
import Test.QuickCheck
import Test.Utils

import Models
import GameLogic


testIsBoardFullAssertion =
  (isBoardFull $ emptyBoard) @=? False


testFindWinner :: Assertion
testFindWinner = undefined


