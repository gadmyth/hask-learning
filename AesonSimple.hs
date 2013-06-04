{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy.Char8
import GHC.Generics (Generic)


data MetaData = MetaData {
     url :: String,
     title :: String
} deriving (Show, Generic)

instance FromJSON MetaData

data Properties = Propeties {
     detail :: String, 
     mag :: Double
} deriving (Show, Generic)

instance FromJSON Properties

data Feature = Feature {
     id :: String, 
     properties :: Properties
} deriving (Show, Generic)

instance FromJSON Feature

data Feed = Feed {
     metadata :: MetaData, 
     features :: [Feature]
} deriving (Show, Generic)

instance FromJSON Feed

main :: IO ()
main = do 
     let req = decode "{\"metadata\":{\"url\":\"FeatureCollection\",\"title\":\"USGS All Earthquakes, Past Hour\"},\"features\":[{\"id\":\"ci15351137\", \"properties\":{\"detail\":\"http://earthquake.usgs.gov/earthquakes/feed/v1.0/detail/ci15351137.geojson\", \"mag\":1.1}}]}" :: Maybe Feed
     case req of
          Nothing -> print "parse failed"
          Just f -> print.("the url of metadata is: "++) $ url $ metadata f
