{-# LANGUAGE LambdaCase #-}

-- | Cardinal direction.
module Viewfinder.Direction
  ( Direction (..),
    sample,
    pretty,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Viewfinder.Gen

-- | Cardinal directions, somewhat coarsely defined.
data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | Generate a 'Direction'.
sample :: Gen Direction
sample = oneof (NonEmpty.fromList [minBound .. maxBound])

-- | Pretty-print a 'Direction'.
pretty :: Direction -> String
pretty = \case
  North -> "N"
  NorthEast -> "NE"
  East -> "E"
  SouthEast -> "SE"
  South -> "S"
  SouthWest -> "SW"
  West -> "W"
  NorthWest -> "NW"
