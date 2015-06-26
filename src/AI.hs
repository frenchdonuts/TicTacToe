module AI where

import Data.Maybe (isJust, catMaybes)

import Models
import GameLogic
import Minimax


-- Ai
instance Minimax TicTacToe where
  isEndState = snd . gameOver

  utilityFn state isMax =
    case (gameOver state) of
          (Nothing, _)   -> 0
          (Just p, True) | isMax && (curPlayer == p) -> 1
                         | (not isMax) && (not $ curPlayer == p) -> 1
                         | otherwise -> -1
    where curPlayer = turn state

  children state = catMaybes . filter isJust $ map (playTile state) [1..9]


runAi :: TicTacToe -> String
runAi state@(TicTacToe p _) = show $ computeTileToPlay (mDecision state) state

-- Figure out move to get into nxtState given curState
computeTileToPlay :: TicTacToe -> TicTacToe -> Int
computeTileToPlay nxtState curState = index
  where index = head [i | (i, s) <- zip [1..] possibleMoves, s==(Just nxtState)]
        possibleMoves = map (playTile curState) [1..9]
