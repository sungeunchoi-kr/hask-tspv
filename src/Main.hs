{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Data.Text as Text
import qualified Data.Map as Map

import qualified TSPData as TSPData
import Draw

data Opts = Opts
  { optInputDataFile :: String
  ,  optCitiesDataFile :: String
  , optVersion :: Bool }

sample :: Parser Opts
sample = Opts
      <$> strOption
          ( long "data"
         <> metavar "FILE"
         <> help "Input data file" )

      <*> strOption
          ( long "cities"
         <> metavar "FILE"
         <> help "Cities data file" )

      <*> switch
          ( long "version"
         <> short 'v'
         <> help "Print version information" )

-- 1. parse the csv file: best-dist;worst-dist;[1,2,3,4, .. ,n]
-- https://wiki.haskell.org/GetOpt
--
-- http://andrew.gibiansky.com/blog/haskell/haskell-gloss/

main :: IO ()
main = do
    start =<< execParser opts
    where
        opts = info (sample <**> helper)
          ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "hello - a test for optparse-applicative" )

start :: Opts -> IO ()
start (Opts path citiesDataPath False) = do
    putStrLn $ "Reading from traversals data file " ++ path
    traversalsData <- parseDataFile <$> lines <$> readFile path
    putStrLn $ "Reading from cities file " ++ citiesDataPath
    citiesGraph <- TSPData.loadCitiesDataAsGraph <$> lines <$> readFile citiesDataPath

    putStrLn $ "cities file has " ++ (show $ Map.size citiesGraph) ++ " cities."

    runVisualizer citiesGraph traversalsData
    return ()

start _ = return ()

-- disgusting but will have to do for now.
parseDataFile :: [String] -> [TraversalData]
parseDataFile lines = fmap parseLine lines
    where
        parseLine :: String -> TraversalData
        parseLine line =
            let tokens = Text.splitOn ";" (Text.pack line) 
                best = read $ Text.unpack $ tokens !! 0
                worst = read $ Text.unpack $ tokens !! 1
                traversal' = read $ Text.unpack $ tokens !! 2
                traversal = traversal' ++ [(head traversal')]
            in
            TraversalData best worst traversal 

