module Main where

import Text.Read (readMaybe)

import TicTacToe
import InputOutput

data Terminal = Terminal

instance Input Terminal where
  input _ = getLine
  parseMaybe _ = readMaybe

instance Output Terminal where
  output _ = putStrLn

main :: IO ()
main = do
  ticTacToe Terminal

