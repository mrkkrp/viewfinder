-- | Generation of random 'GridOffset' values.
module Viewfinder.Gen.GridOffset
  ( sampleGridOffset,
  )
where

import Geodetics.Grid
import Viewfinder.Gen

-- | Generate a 'GridOffset' which a random bearing and a distance within
-- the given radius.
sampleGridOffset :: Double -> Gen GridOffset
sampleGridOffset radius = do
  bearing <- (* (pi * 2)) <$> nextDouble
  distance <- (* radius) <$> nextDouble
  return (polarOffset distance bearing)
