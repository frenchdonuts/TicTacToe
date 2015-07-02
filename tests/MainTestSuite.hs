module Main (
  main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Utils

import ModelsTest
import GameLogicTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = undefined
