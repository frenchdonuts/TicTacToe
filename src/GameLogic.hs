module GameLogic where

import Control.Applicative (pure, liftA2, (<*))
import Data.Maybe (isJust)

import Models


-- State transition(s)
playTile :: TicTacToe -> Int -> Maybe TicTacToe
playTile game@(TicTacToe p b) i
  -- Check if ith tile has already been taken
  | (getPlayerOnTile i b) /= Nothing = Nothing
  -- Play tile
  | otherwise = liftA2 TicTacToe (pure $ cycle p) (setPlayerOnTile i b (Just p))
  where cycle X = O
        cycle O = X
--


-- Terminal state checks
gameOver :: TicTacToe -> (Maybe Player, Bool)
gameOver (TicTacToe _ b) = f (findWinner b) (isBoardFull b)
  where f Nothing b = (Nothing, b)  -- No one won
        f xOrO _ = (xOrO, True)     -- Someone won

isBoardFull :: Board -> Bool
isBoardFull b = isJust $ foldr (<*) (Just X) allTiles
  where allTiles = map (`getPlayerOnTile` b) [1..9]

findWinner :: Board -> Maybe Player
findWinner b = foldr stepFn Nothing ( (rows b) ++ (columns b) ++ (diagonals b) )
  where stepFn [Just X, Just X, Just X] acc = Just X
        stepFn [Just O, Just O, Just O] acc = Just O
        stepFn _ acc = acc
--

