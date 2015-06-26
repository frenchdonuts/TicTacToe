module GameLogic where

import Control.Applicative (pure, liftA2, (<*>), (<*), (<$>))
import Data.Maybe (isJust, catMaybes)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

import Models


-- State transition
playTile :: TicTacToe -> Int -> Maybe TicTacToe
playTile game@(TicTacToe p b) i
  -- Check if ith tile has already been taken
  | (playerOnTile i b) /= Nothing = Nothing
  -- Play tile
  | otherwise = liftA2 TicTacToe (pure $ cycle p) (setTile i b p)
  where cycle X = O
        cycle O = X

setTile :: Int -> Board -> Player -> Maybe Board
setTile i board p' =
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
--


-- Terminal state checks
gameOver :: TicTacToe -> (Maybe Player, Bool)
gameOver (TicTacToe _ b) = f (checkForWin b) (isBoardFull b)
  where f Nothing b = (Nothing, b)  -- No one won
        f xOrO _ = (xOrO, True)     -- Someone won

isBoardFull :: Board -> Bool
isBoardFull b = isJust $ foldr (<*) (Just X) allTiles
  where allTiles = map (`playerOnTile` b) [1..9]

checkForWin :: Board -> Maybe Player
checkForWin b = foldr stepFn Nothing reifiedConfigs
  where stepFn [Just X, Just X, Just X] acc = Just X
        stepFn [Just O, Just O, Just O] acc = Just O
        stepFn _ acc = acc
        reifiedConfigs = (map . map) ($ b) winConfigs
winConfigs = [[t1,t2,t3],[t4,t5,t6],[t7,t8,t9], -- Rows
              [t1,t4,t7],[t2,t5,t8],[t3,t6,t9], -- Columns
              [t1,t5,t9],[t3,t5,t7]]            -- Diagonals
--

