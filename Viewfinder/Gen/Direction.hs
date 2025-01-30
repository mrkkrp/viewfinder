-- | Generation of random 'Direction' values.
module Viewfinder.Gen.Direction (sampleDirection) where

import Data.List.NonEmpty qualified as NonEmpty
import Viewfinder.Direction
import Viewfinder.Gen

-- | Generate a 'Direction'.
sampleDirection :: Gen Direction
sampleDirection = oneof (NonEmpty.fromList [minBound .. maxBound])
