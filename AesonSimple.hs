{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8
import GHC.Generics (Generic)
import Control.Applicative


data MetaData = MetaData {
     url :: String,
     title :: String
} deriving (Show, Generic)

instance FromJSON MetaData where
         parseJSON (Object o) = MetaData <$> o .: "url" <*> o .: "title" 

data Properties = Properties {
     detail :: String, 
     mag :: Double
} deriving (Show, Generic)

instance FromJSON Properties where
         parseJSON (Object o) = Properties <$> o .: "detail" <*> o .: "mag"

data Feature = Feature {
     id :: String, 
     properties :: Properties
} deriving (Show, Generic)

instance FromJSON Feature where
         parseJSON (Object o) = Feature <$> o .: "id" <*> o .: "properties"

data Feed = Feed {
     metadata :: MetaData, 
     features :: [Feature]
} deriving (Show, Generic)

instance FromJSON Feed where
         parseJSON (Object o) = Feed <$> o .: "metadata" <*> o .: "features"

main :: IO ()
main = do 
     let req = decode "{\"metadata\":{\"url\":\"FeatureCollection\",\"title\":\"USGS All Earthquakes, Past Hour\"},\"features\":[{\"id\":\"ci15351137\", \"properties\":{\"detail\":\"http://earthquake.usgs.gov/earthquakes/feed/v1.0/detail/ci15351137.geojson\", \"mag\":1.1}}]}" :: Maybe Feed
     case req of
          Nothing -> print "parse failed"
          Just f -> print.("the url of metadata is: "++) $ url $ metadata f
