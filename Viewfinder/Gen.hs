{-# LANGUAGE DerivingVia #-}

-- | This module defines a generator monad.
module Viewfinder.Gen
  ( Gen,
    run,
    nextDouble,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Functor.Identity (Identity (..))
import System.Random.SplitMix (SMGen)
import System.Random.SplitMix qualified as SplitMix

-- | A monad that supports preudo-random generation of values.
newtype Gen a = Gen (SMGen -> (a, SMGen))
  deriving (Functor, Applicative, Monad) via (StateT SMGen Identity)

-- | Run the 'Gen' monad.
run :: SMGen -> Gen a -> (a, SMGen)
run g (Gen f) = f g

-- | Generate a 'Double' within the @[0, 1)@ range.
nextDouble :: Gen Double
nextDouble = Gen SplitMix.nextDouble
