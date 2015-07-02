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

getPlayerOnTile :: Int -> (Board -> Maybe Player)
getPlayerOnTile i =
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
setPlayerOnTile :: Int -> Board -> Player -> Maybe Board
setPlayerOnTile i board p' =
  case i of
    1 -> Just board{ t1 = p }
    2 -> Just board{ t2 = p }
    3 -> Just board{ t3 = p }
    4 -> Just board{ t4 = p }
    5 -> Just board{ t5 = p }
    6 -> Just board{ t6 = p }
    7 -> Just board{ t7 = p }
    8 -> Just board{ t8 = p }
    9 -> Just board{ t9 = p }
    otherwise -> Nothing -- Index out of bounds
  where p = Just p'


data TicTacToe = TicTacToe {
  turn :: Player,
  board :: Board
} deriving (Eq)

--


