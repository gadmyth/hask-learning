{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client

main::IO ()
main = do
     get "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson" justTheBodyHandler

justTheBodyHandler :: Response -> InputStream S.ByteString -> IO ()
justTheBodyHandler rep i = do
                   --Streams.connect i stdout
                   putStr $ show rep
