module TicTacToe where

import Text.Read (readMaybe)

import Models
import GameLogic
import Minimax
import AI


class TicTacToeInterface a where
  onOneOrTwoPlayerPrompt :: a -> IO ()

  onPickTilePrompt :: a -> TicTacToe -> IO ()

  onGameTied :: a -> TicTacToe -> IO ()

  onGameWon :: a -> TicTacToe -> Player -> IO ()

  displayGameState :: a -> TicTacToe -> IO ()

  onInvalidChoice :: a -> IO ()

  getUserInput :: a -> IO String



-- Engine
ticTacToe :: (TicTacToeInterface a) => a -> IO ()
ticTacToe ioDevice = do
  (onOneOrTwoPlayerPrompt ioDevice)
  oneOrTwo <- (getUserInput ioDevice)
  case (readMaybe oneOrTwo) of
    Just 1    -> playGame initGame True randomBoolean ioDevice
    Just 2    -> playGame initGame False False ioDevice

    otherwise -> (onInvalidChoice ioDevice) >> ticTacToe ioDevice
  where randomBoolean = False

playGame :: (TicTacToeInterface a) => TicTacToe -> Bool -> Bool -> a -> IO ()
playGame game aiEnabled playerIsAi ioDevice = do
  (onPickTilePrompt ioDevice) game
  (displayGameState ioDevice) game
  tile <- if (aiEnabled && playerIsAi)
             then (return $ runAi game)
             else (getUserInput ioDevice)
  case ( readMaybe tile >>= (playTile game) ) of
    (Just game') -> recurseOrEnd (gameOver game') game' aiEnabled playerIsAi ioDevice

    Nothing      -> (onInvalidChoice ioDevice) >>
                    playGame game aiEnabled playerIsAi ioDevice

recurseOrEnd p game' aiEnabled playerIsAi ioDevice =
  case p of
    (_, False)      -> playGame game' aiEnabled (not playerIsAi) ioDevice

    (Nothing, True) -> (onGameTied ioDevice) game' >> --("Tie game.")
                       (displayGameState ioDevice) game'

    (Just p, True)  -> (onGameWon ioDevice) game' p >>
                       (displayGameState ioDevice) game'
--
