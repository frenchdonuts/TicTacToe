module GameLogicTest where


import Test.HUnit
import Test.QuickCheck
import Test.Utils

import Models
import GameLogic


-- testIsBoardFull :: Assertion
testIsBoardFullAssertion =
  (isBoardFull $ emptyBoard) @=? False


testCheckForWin :: Assertion
testCheckForWin = undefined


