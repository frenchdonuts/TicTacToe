module Main where

import Text.Read (readMaybe)

import TicTacToe
import Models


main :: IO ()
main = do
  putStrLn "One Player(1) or two players(2)?"
  oneOrTwo <- getLine
  case (readMaybe oneOrTwo) of
    Just 1    -> playGame initGame True randomBoolean
    Just 2    -> playGame initGame False False
    otherwise -> putStrLn "Invalid choice." >> main
  where randomBoolean = False


emptyBoard = Board {
  t1 = Nothing,
  t2 = Nothing,
  t3 = Nothing,
  t4 = Nothing,
  t5 = Nothing,
  t6 = Nothing,
  t7 = Nothing,
  t8 = Nothing,
  t9 = Nothing
}
initGame = TicTacToe {
  turn = X,
  board = emptyBoard
}


--
testState0 = TicTacToe {       -- X Wins
  turn = O,
  board = Board {
     t1 = Just X,
     t2 = Just O,
     t3 = Just X,
     t4 = Just O,
     t5 = Just X,
     t6 = Just O,
     t7 = Just X,
     t8 = Nothing,
     t9 = Nothing}}
testState1 = TicTacToe {     -- Tie state
  turn = O,
  board = Board {
     t1 = Just X,
     t2 = Just X,
     t3 = Just O,
     t4 = Just O,
     t5 = Just O,
     t6 = Just X,
     t7 = Just X,
     t8 = Just O,
     t9 = Just O}}
testState2 = TicTacToe {     -- X About to Win
  turn = O,
  board = Board {
     t1 = Just X,
     t2 = Just X,
     t3 = Nothing,
     t4 = Nothing,
     t5 = Nothing,
     t6 = Nothing,
     t7 = Nothing,
     t8 = Nothing,
     t9 = Just O}}

