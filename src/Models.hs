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

setPlayerOnTile :: Int -> Board -> Maybe Player -> Maybe Board
setPlayerOnTile i board p =
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

rows :: Board -> [[Maybe Player]]
rows b = (map . map) ($ b) rs
  where rs = [[t1,t2,t3],[t4,t5,t6],[t7,t8,t9]]

columns :: Board -> [[Maybe Player]]
columns b = (map . map) ($ b) cs
  where cs = [[t1,t4,t7],[t2,t5,t8],[t3,t6,t9]]

diagonals :: Board -> [[Maybe Player]]
diagonals b = (map . map) ($ b) ds
  where ds = [[t1,t5,t9],[t3,t5,t7]]

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


data TicTacToe = TicTacToe {
  turn :: Player,
  board :: Board
} deriving (Eq)

initGame = TicTacToe {
  turn = X,
  board = emptyBoard
}
--

