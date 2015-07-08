module Presentation where

import Models


instance Show TicTacToe where
  show s =
    "Turn: " ++ (show (turn s)) ++ "\n" ++
    (show (board s))

instance Show Board where
  show b =
    (f $ t1 b) ++ "  " ++ (f $ t2 b) ++ "  " ++ (f $ t3 b) ++ "\n" ++
    (f $ t4 b) ++ "  " ++ (f $ t5 b) ++ "  " ++ (f $ t6 b) ++ "\n" ++
    (f $ t7 b) ++ "  " ++ (f $ t8 b) ++ "  " ++ (f $ t9 b) ++ "\n"
      where f Nothing = "_"
            f (Just X) = "X"
            f (Just O) = "O"

instance Show Player where
  show X = "X"
  show O = "O"
