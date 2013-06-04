{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy.Char8
import GHC.Generics (Generic)

import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Http.Client

import Control.Applicative ((<$>))

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
     print $ ("the url of metadata is: "++) <$> url <$> metadata <$> req

     c <- openConnection "earthquake.usgs.gov" 80
     q <- buildRequest $ do
       http GET "/earthquakes/feed/v1.0/summary/all_day.geojson"
       setAccept "application/json"

     sendRequest c q emptyBody
     x <- receiveResponse c jsonHandler
     print $ url <$> metadata <$> x


jsonHandler :: Response -> InputStream S.ByteString -> IO (Maybe Feed)
jsonHandler p i = do
            chunks <- Streams.toList i
            let feed = decode (BL.fromChunks chunks) :: Maybe Feed
            return feed
