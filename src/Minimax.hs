module Minimax where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

-- Video on minimax algorithm:
-- https://www.youtube.com/watch?v=OkP8BAwfO24

class Minimax a where
  isEndState :: a -> Bool
  utilityFn :: (Num c) => a -> Bool -> c
  children :: a -> [a]

mDecision :: Minimax a => a -> a
mDecision state =
  fst $ maximumBy (comparing snd) (map f (children state))
    where f child = (child, mMin child)

mMax :: Minimax a => a -> Int
mMax state
  | isEndState state = utilityFn state True
  | otherwise = maximum $ map mMin (children state)

mMin :: Minimax a => a -> Int
mMin state
  | isEndState state = utilityFn state False
  | otherwise = minimum $ map mMax (children state)

