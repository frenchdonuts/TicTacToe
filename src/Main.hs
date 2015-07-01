module Main where

import Text.Read (readMaybe)

import TicTacToe
import Presentation


data Terminal = Terminal

instance TicTacToeInterface Terminal where
  onOneOrTwoPlayerPrompt _ = putStrLn "One Player(1) or two players(2)?"

  onPickTilePrompt _ _ = putStrLn "Please pick a tile (1-9)."

  onGameTied _ _ = putStrLn "Game tied."

  onGameWon _ _ p = putStrLn $ "Player " ++ (show p) ++ " won!"

  displayGameState _ game = putStrLn $ show game

  onInvalidChoice _ = putStrLn "Invalid choice."

  getUserInput _ = getLine


main :: IO ()
main = do
  ticTacToe Terminal

