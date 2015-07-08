module Presentation where

import Models


instance Show TicTacToe where
  show s =
    "Turn: " ++ (show (turn s)) ++ "\n" ++
    (show (board s))

instance Show Board where
  show b =
    (show (t1 b)) ++ "  " ++ (show (t2 b)) ++ "  " ++ (show (t3 b)) ++ "\n" ++
    (show (t4 b)) ++ "  " ++ (show (t5 b)) ++ "  " ++ (show (t6 b)) ++ "\n" ++
    (show (t7 b)) ++ "  " ++ (show (t8 b)) ++ "  " ++ (show (t9 b)) ++ "\n"

instance Show Player where
  show Nothing = " "
  show (Just X) = "X"
  show (Just O) = "O"
