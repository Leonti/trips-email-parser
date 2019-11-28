{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Airports where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics (Generic)

data Airport = Airport { 
    airportId :: !String
  , name :: !String
  , city :: !String
  , country :: !String
  , iata :: !String 

  , icao :: ()
  , latitude :: ()
  , longitude :: ()
  , altitude :: ()
  , timezone :: () 

  , dst :: ()
  , timezoneTz :: ()
  , type' :: ()
  , source :: ()
  } deriving (Generic, Show)

instance FromRecord Airport

airports :: IO (Either String (V.Vector Airport))
airports = do
  csvData <- BL.readFile "airports.csv"
  return $ decode NoHeader csvData