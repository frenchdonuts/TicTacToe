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

instance Show Board where
  show b =
    (show (t1 b)) ++ "  " ++ (show (t2 b)) ++ "  " ++ (show (t3 b)) ++ "\n" ++
    (show (t4 b)) ++ "  " ++ (show (t5 b)) ++ "  " ++ (show (t6 b)) ++ "\n" ++
    (show (t7 b)) ++ "  " ++ (show (t8 b)) ++ "  " ++ (show (t9 b)) ++ "\n"

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


data TicTacToe = TicTacToe {
  turn :: Player,
  board :: Board
} deriving (Eq)

instance Show TicTacToe where
  show s =
    "Turn: " ++ (show (turn s)) ++ "\n" ++
    (show (board s))
