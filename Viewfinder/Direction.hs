-- | Cardinal direction.
module Viewfinder.Direction
  ( Direction (..),
  )
where

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
