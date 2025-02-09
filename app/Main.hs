{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (forM_, replicateM, when)
import Data.Version (showVersion)
import Development.GitRev
import Geodetics.Geodetic
import Numeric.Natural
import Options.Applicative
import Paths_viewfinder (version)
import System.Random.SplitMix
import Viewfinder.Gen qualified as Gen
import Viewfinder.View qualified as View

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  g <- case optSeed of
    Nothing -> initSMGen
    Just seed -> return (mkSMGen (fromIntegral seed))
  let (views, _) =
        Gen.run g $
          replicateM
            (fromIntegral optViewsToGenerate)
            (View.sample (fromIntegral optRadius) optOrigin)
      m :: Int
      m =
        1 + floor (logBase 10.0 (fromIntegral optViewsToGenerate :: Double))
  forM_ (zip [1 ..] views) $ \(i :: Int, view) -> do
    when optPrintIndices $ do
      let i' = show i
          m' = m - length i'
      putStr (replicate m' ' ' ++ i' ++ ". ")
    putStrLn (View.pretty view)

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.
data Opts = Opts
  { -- | Seed
    optSeed :: Maybe Natural,
    -- | The number of views to generate
    optViewsToGenerate :: Natural,
    -- | Maximal distance from the origin
    optRadius :: Natural,
    -- | Print indices of the generated views.
    optPrintIndices :: Bool,
    -- | Location of the origin
    optOrigin :: Geodetic WGS84
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "A utility for randomized selection of views"
    ]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      unwords
        [ "viewfinder",
          showVersion version,
          $gitBranch,
          $gitHash
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (optional . option auto . mconcat)
      [ long "seed",
        short 's',
        metavar "SEED",
        help "Seed of the preudo-random generator (if not set a random seed is used)"
      ]
    <*> (option auto . mconcat)
      [ short 'n',
        metavar "N",
        value 1,
        showDefault,
        help "Number of views to generate"
      ]
    <*> (option auto . mconcat)
      [ long "radius",
        short 'r',
        metavar "R",
        value 3000,
        showDefault,
        help "Maximal distance from the origin in meters"
      ]
    <*> (switch . mconcat)
      [ long "indices",
        short 'i',
        help "Print indices of the generated views"
      ]
    <*> (argument parseGeodetic . mconcat)
      [ metavar "ORIGIN",
        help "Location of the origin (latitude and longitude as a single space-separated string)"
      ]

-- | Parse a 'Geodetic' value.
parseGeodetic :: ReadM (Geodetic WGS84)
parseGeodetic = eitherReader $ \s ->
  case readGroundPosition WGS84 s of
    Nothing -> Left ("Invalid coordinate: " ++ s)
    Just x -> Right x
