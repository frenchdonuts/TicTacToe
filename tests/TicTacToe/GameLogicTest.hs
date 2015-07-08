module GameLogicTest where


import Test.HUnit
import Test.QuickCheck
import Test.Utils

import Models
import GameLogic
import Presentation

import ModelsTest

-- newtype WinningBoard = WinningBoard Board deriving (Eq, Show)
-- instance Arbitrary WinningBoard where
--   arbitrary = do

--     p <- arbitrary
--     let b = emptyBoard
--     rowColumnOrDiag <- oneof [return [1,2,3], return [4,5,6], return [7,8,9]]
--     return $ WinningBoard (setTiles p b rowColumnOrDiag)

--     where setTiles p b rcd = foldl (\acc i -> setPlayerOnTile i acc p) b rcd

-- isBoardFull :: Board -> Bool
assertEmptyBoardIsNotFull =
  (isBoardFull $ emptyBoard) @=? False

propNoNothingsInFullBoard b =
  (isBoardFull b) == (numberOfNothings b == 0)
  where numberOfNothings = sum . (map f) . concat . rows
        f Nothing = 1
        f _ = 0

-- findWinner :: Board -> Maybe Player
testFindWinner1 =
  findWinner winState @=? (Just X)

testFindWinner2 =
  findWinner tieState @=? Nothing

winState = Board {      -- X Wins
   t1 = Just X,
   t2 = Just O,
   t3 = Just X,
   t4 = Just O,
   t5 = Just X,
   t6 = Just O,
   t7 = Just X,
   t8 = Nothing,
   t9 = Nothing }

tieState = Board {      -- Tie State
   t1 = Just X,
   t2 = Just X,
   t3 = Just O,
   t4 = Just O,
   t5 = Just O,
   t6 = Just X,
   t7 = Just X,
   t8 = Just O,
   t9 = Just O }
-- Generate a list winning and tie board states
