module Models where

-- Models
data Player = X | O deriving (Show, Eq)


data Board = Board {
  t1 :: Maybe Player,
  t2 :: Maybe Player,
  t3 :: Maybe Player,
  t4 :: Maybe Player,
  t5 :: Maybe Player,
  t6 :: Maybe Player,
  t7 :: Maybe Player,
  t8 :: Maybe Player,
  t9 :: Maybe Player
} deriving (Eq)

playerOnTile :: Int -> (Board -> Maybe Player)
playerOnTile i =
  case i of
     1 -> t1
     2 -> t2
     3 -> t3
     4 -> t4
     5 -> t5
     6 -> t6
     7 -> t7
     8 -> t8
     9 -> t9
     otherwise -> const Nothing


data TicTacToe = TicTacToe {
  turn :: Player,
  board :: Board
} deriving (Eq)

--


