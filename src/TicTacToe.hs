module TicTacToe where

import Text.Read (readMaybe)

import InputOutput
import Models
import GameLogic
import Minimax
import AI


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
initGame = TicTacToe {
  turn = X,
  board = emptyBoard
}

-- Engine
ticTacToe :: (Input a, Output a) => a -> IO ()
ticTacToe ioDevice = do
  (output ioDevice) "One Player(1) or two players(2)?"
  oneOrTwo <- (input ioDevice)
  case ((parseMaybe ioDevice) oneOrTwo) of
    Just 1    -> playGame initGame True randomBoolean ioDevice
    Just 2    -> playGame initGame False False ioDevice
    otherwise -> (output ioDevice) "Invalid choice." >> ticTacToe ioDevice
  where randomBoolean = False

playGame :: (Input a, Output a) => TicTacToe -> Bool -> Bool -> a -> IO ()
playGame game aiEnabled playerIsAi ioDevice = do
  (output ioDevice) "Please pick a tile (1-9)."
  (output ioDevice) $ show game
  tile <- if (aiEnabled && playerIsAi)
             then (return $ runAi game)
             else (input ioDevice)
  case ( (parseMaybe ioDevice) tile >>= (playTile game) ) of
    (Just game') -> recurseOrEnd (gameOver game') game' aiEnabled playerIsAi ioDevice

    Nothing      -> putStrLn "Invalid choice." >>
                    playGame game aiEnabled playerIsAi ioDevice

recurseOrEnd p game' aiEnabled playerIsAi ioDevice =
  case p of
    (_, False)      -> playGame game' aiEnabled (not playerIsAi) ioDevice
    (Nothing, True) -> (output ioDevice) ("Tie game.")
    (Just p, True)  -> (output ioDevice) (show p ++ " wins!") >>
                       (output ioDevice) (show game')
--
