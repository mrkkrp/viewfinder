{-# LANGUAGE DerivingVia #-}

-- | This module defines a generator monad.
module Viewfinder.Gen
  ( Gen,
    run,
    nextDouble,
    oneof,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty, (!!))
import System.Random.SplitMix (SMGen)
import System.Random.SplitMix qualified as SplitMix
import Prelude hiding ((!!))

-- | A monad that supports preudo-random generation of values.
newtype Gen a = Gen (SMGen -> (a, SMGen))
  deriving (Functor, Applicative, Monad) via (StateT SMGen Identity)

-- | Run the 'Gen' monad.
run :: SMGen -> Gen a -> (a, SMGen)
run g (Gen f) = f g

-- | Generate a 'Double' within the @[0, 1)@ range.
nextDouble :: Gen Double
nextDouble = Gen SplitMix.nextDouble

-- | Pick a random element from the given non-empty list.
oneof :: NonEmpty a -> Gen a
oneof xs = Gen $ \g -> do
  let (i, g') = SplitMix.nextInteger 0 (fromIntegral (length xs - 1)) g
   in (xs !! fromIntegral i, g')
