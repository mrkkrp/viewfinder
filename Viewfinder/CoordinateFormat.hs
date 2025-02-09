{-# LANGUAGE LambdaCase #-}

module Viewfinder.CoordinateFormat
  ( CoordinateFormat (..),
    parse,
    render,
  )
where

import Options.Applicative (ReadM, eitherReader)

-- | How to output geographic coordinates.
data CoordinateFormat
  = -- | 34° 31' 23.52\" N, 46° 13' 56.43\" W
    DegreesMinutesSeconds
  | -- | 34.52327, -46.23234
    SignedDecimals
  | -- | 34.52327N, 46.23234W
    NSEWDecimals
  | -- | 343123.52N, 0461356.43W
    DDDMMSS
  deriving (Show)

-- | Convert a 'String' to a 'CoordinateFormat'.
parse :: ReadM CoordinateFormat
parse = eitherReader $ \case
  "dms" -> Right DegreesMinutesSeconds
  "sdecimals" -> Right SignedDecimals
  "udecimals" -> Right NSEWDecimals
  "dddmmss" -> Right DDDMMSS
  x -> Left $ "Unknown coordinate format: " ++ x

-- | Convert 'CoordinateFormat' into a human-friendly 'String'
-- representation.
render :: CoordinateFormat -> String
render = \case
  DegreesMinutesSeconds -> "dms"
  SignedDecimals -> "sdecimals"
  NSEWDecimals -> "udecimals"
  DDDMMSS -> "dddmmss"
