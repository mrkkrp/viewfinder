-- | Utilities for manipulation of 'Geodetic' values.
module Viewfinder.Geodetic (applyGridOffset) where

import Geodetics.Ellipsoids (Ellipsoid)
import Geodetics.Geodetic
import Geodetics.Grid
import Geodetics.TransverseMercator

-- | Apply a 'GridOffset' to a 'Geodetic'.
applyGridOffset :: (Ellipsoid e) => GridOffset -> Geodetic e -> Geodetic e
applyGridOffset offset g =
  fromGrid (applyOffset offset (toGrid transverseMercatorGrid g))
  where
    transverseMercatorGrid = mkGridTM g mempty 1.0
