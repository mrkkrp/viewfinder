module Viewfinder.View
  ( View (..),
    sample,
    pretty,
  )
where

import Geodetics.Geodetic
import Geodetics.Grid
import Geodetics.TransverseMercator
import Viewfinder.Direction (Direction)
import Viewfinder.Direction qualified as Direction
import Viewfinder.Gen

-- | A 'View' as a combination of a geographic coordinate and a 'Direction'.
data View = View (Geodetic WGS84) Direction
  deriving (Show)

-- | Generate a random 'View' whose location is within the radius from the
-- origin.
sample ::
  -- | Radius in meters
  Double ->
  -- | Coordinates of the origin
  Geodetic WGS84 ->
  Gen View
sample radius origin = do
  offset <- sampleGridOffset radius
  direction <- Direction.sample
  return (View (applyGridOffset offset origin) direction)

-- | Generate a 'GridOffset' which a random bearing and a distance within
-- the given radius.
sampleGridOffset :: Double -> Gen GridOffset
sampleGridOffset radius = do
  bearing <- (* (pi * 2)) <$> nextDouble
  distance <- (* radius) <$> nextDouble
  return (polarOffset distance bearing)

-- | Apply a 'GridOffset' to a 'Geodetic'.
applyGridOffset :: GridOffset -> Geodetic WGS84 -> Geodetic WGS84
applyGridOffset offset g =
  fromGrid (applyOffset offset (toGrid transverseMercatorGrid g))
  where
    transverseMercatorGrid = mkGridTM g mempty 1.0

-- | Pretty-print a 'View'.
pretty :: View -> String
pretty (View coordinate direction) =
  showGeodeticNSEWDecimal coordinate ++ " | " ++ Direction.pretty direction
