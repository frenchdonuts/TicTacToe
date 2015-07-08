{-# LANGUAGE FlexibleInstances #-}
module ModelsTest where

import Control.Applicative ((<$>),(<*>))

import Test.HUnit
import Test.QuickCheck
import Test.Utils

import Models


newtype Int1Through9 = Int1Through9 Int deriving (Eq, Show)
instance Arbitrary Int1Through9 where
  arbitrary = Int1Through9 <$> choose (1, 9)

instance Arbitrary (Maybe Player) where
  arbitrary = oneof [return Nothing, return (Just X), return (Just O)]

instance Arbitrary Board where
  arbitrary =
    Board <$>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary


propOutOfBoundsGetIsNothing i b =
  (i < 1) || (i > 9) ==>
    getPlayerOnTile i b == Nothing


propOutOfBoundsSetIsNothing p i b =
  (i < 1) || (i > 9) ==>
    setPlayerOnTile i b p == Nothing


propSetThenGetPlayerEqual p (Int1Through9 i) b =
    (setPlayerOnTile i b p >>= getPlayerOnTile i) == p


propGetThenSetPlayerEqual b (Int1Through9 i) =
    ((setPlayerOnTile i b) $ getPlayerOnTile i b) == (Just b)


