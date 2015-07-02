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
  | otherwise = liftA2 TicTacToe (pure $ cycle p) (setPlayerOnTile i b p)
  where cycle X = O
        cycle O = X
--


-- Terminal state checks
gameOver :: TicTacToe -> (Maybe Player, Bool)
gameOver (TicTacToe _ b) = f (checkForWin b) (isBoardFull b)
  where f Nothing b = (Nothing, b)  -- No one won
        f xOrO _ = (xOrO, True)     -- Someone won

isBoardFull :: Board -> Bool
isBoardFull b = isJust $ foldr (<*) (Just X) allTiles
  where allTiles = map (`getPlayerOnTile` b) [1..9]

checkForWin :: Board -> Maybe Player
checkForWin b = foldr stepFn Nothing reifiedConfigs
  where stepFn [Just X, Just X, Just X] acc = Just X
        stepFn [Just O, Just O, Just O] acc = Just O
        stepFn _ acc = acc
        reifiedConfigs = (map . map) ($ b) winConfigs
winConfigs = [[t1,t2,t3],[t4,t5,t6],[t7,t8,t9], -- Rows
              [t1,t4,t7],[t2,t5,t8],[t3,t6,t9], -- Columns
              [t1,t5,t9],[t3,t5,t7]]            -- Diagonals
-- rowIndices [[1,2,...n],[n+1,n+2,...2n],...[(n-1)n+1,(n-1)n+2,...(n*n)]]
-- columnIndices [[1, 1+n, 1+2n,...1+(n-1)n],...[n, n+n, n+2n,...n+(n-1)n]]
-- diagonalIndices
--

