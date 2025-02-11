{-# LANGUAGE OverloadedStrings #-}

module Viewfinder.View
  ( View (..),
    sampleN,
    sample,
    render,
    renderCsv,
    optimizeTrajectory,
  )
where

import Algorithm.Search (dijkstraAssoc)
import Control.Monad (replicateM)
import Data.ByteString qualified as Strict
import Data.ByteString.Lazy qualified as Lazy
import Data.Csv (ToNamedRecord, (.=))
import Data.Csv qualified as Csv
import Data.Function (on)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Geodetics.Geodetic
import Geodetics.Grid
import Geodetics.TransverseMercator
import Numeric.Natural
import Viewfinder.CoordinateFormat (CoordinateFormat (..))
import Viewfinder.Direction (Direction)
import Viewfinder.Direction qualified as Direction
import Viewfinder.Gen

-- | A 'View' as a combination of a geographic coordinate and a 'Direction'.
data View = View (Geodetic WGS84) Direction
  deriving (Show)

instance Eq View where
  (==) = (==) `on` makeComparable

instance Ord View where
  compare = compare `on` makeComparable

-- | Isomorphism to something that compares.
makeComparable :: View -> (Double, Double, Double, Direction)
makeComparable (View g d) = (latitude g, longitude g, geoAlt g, d)

-- | A 'View' equipped with 'CoordinateFormat'.
data ViewWithCoordinateFormat = ViewWithCoordinateFormat CoordinateFormat View

instance ToNamedRecord ViewWithCoordinateFormat where
  toNamedRecord
    ( ViewWithCoordinateFormat
        coordinateFormat
        (View coordinate direction)
      ) =
      Csv.namedRecord
        [ coordinateField .= renderCoordinate coordinateFormat coordinate,
          directionField .= Direction.render direction
        ]

-- | Generate a number of random 'View's whose location is within the radius
-- from the origin.
sampleN ::
  -- | The number of views to generate
  Natural ->
  -- | Radius in meters
  Double ->
  -- | Coordinates of the origin
  Geodetic WGS84 ->
  Gen [View]
sampleN viewsToGenerate radius origin =
  replicateM
    (fromIntegral viewsToGenerate)
    (sample radius origin)

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

-- | Render a 'View' in a human-friendly way.
render :: CoordinateFormat -> View -> String
render coordinateFormat (View coordinate direction) =
  renderCoordinate coordinateFormat coordinate ++ " | " ++ Direction.render direction

-- | Render a 'Geodetic' as a latitude/longitude coordinate pair in the
-- specified 'CoordinateFormat'.
renderCoordinate :: CoordinateFormat -> Geodetic WGS84 -> String
renderCoordinate coordinateFormat coordinate =
  case coordinateFormat of
    DegreesMinutesSeconds -> showGeodeticLatLong coordinate
    SignedDecimals -> showGeodeticSignedDecimal coordinate
    NSEWDecimals -> showGeodeticNSEWDecimal coordinate
    DDDMMSS -> showGeodeticDDDMMSS True coordinate

-- | Render a collection of 'View's as a CSV document.
renderCsv :: CoordinateFormat -> [View] -> Lazy.ByteString
renderCsv coordinateFormat =
  Csv.encodeByName header . fmap (ViewWithCoordinateFormat coordinateFormat)
  where
    header = Vector.fromList [coordinateField, directionField]

-- | Order given 'View's so that they form an optimal trajectory from and
-- back to origin. This is somewhat naïve, since we assume that any two
-- positions can be connected by a straight line. In reality navigation will
-- likely be more complex.
optimizeTrajectory ::
  -- | Origin
  Geodetic WGS84 ->
  -- | 'View's to order
  [View] ->
  -- | 'View's ordered to form an optimal trajectory
  [View]
optimizeTrajectory _ [] = []
optimizeTrajectory _ [x] = [x]
optimizeTrajectory origin viewsToOrder =
  case dijkstraAssoc nextStates solutionFound initialState of
    Nothing -> error "Viewfinder.View.optimizeTrajectory: optimization cannot be found"
    Just (_totalCost, states) -> reverse (fst (last states))
  where
    nextStates (viewsSoFar, pendingViews) =
      let mkNextState nextView =
            (nextView : viewsSoFar, Set.delete nextView pendingViews)
       in attachCost . mkNextState <$> Set.toList pendingViews
    attachCost (viewsSoFar, pendingViews) =
      let -- If we are attaching a cost to the last view we need to account
          -- for the way back to origin.
          wayBackFrom (View viewCoordinate _direction) =
            if Set.null pendingViews
              then geodeticDistance viewCoordinate origin
              else 0
          cost = case viewsSoFar of
            [] -> error "Viewfinder.View.optimizeTrajectory: attaching cost to empty path"
            [x@(View viewCoordinate _direction)] ->
              -- This is the first point, hence calculate the distance
              -- between this point and the origin.
              geodeticDistance origin viewCoordinate + wayBackFrom x
            (x : y : _) -> viewDistance x y + wayBackFrom x
       in ((viewsSoFar, pendingViews), cost)
    solutionFound (_viewsSoFar, pendingViews) = Set.null pendingViews
    initialState = ([], Set.fromList viewsToOrder)

-- | Calculate distance between two 'View's.
viewDistance :: View -> View -> Double
viewDistance (View x _) (View y _) = geodeticDistance x y

-- | Calculate distance between two geographic coordinates.
geodeticDistance :: Geodetic WGS84 -> Geodetic WGS84 -> Double
geodeticDistance x y =
  case groundDistance x y of
    Nothing -> error "Viewfinder.View.distance: algorithm failed to converge"
    Just (d, _, _) -> d

coordinateField, directionField :: Strict.ByteString
coordinateField = "coordinate"
directionField = "direction"
