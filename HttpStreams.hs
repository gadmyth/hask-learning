{-# LANGUAGE OverloadedStrings #-}

{-| doc: http://research.operationaldynamics.com/projects/http-streams/doc/Network-Http-Client.html
    url: gist.github.com/joshrotenberg/5666409#file-httpstreamssimple-hs
-}

module Main where

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client

main :: IO ()
main = do
     c <- openConnection "earthquake.usgs.gov" 80
     q <- buildRequest $ do
       http GET "/earthquakes/feed/v1.0/summary/all_day.geojson"
       setAccept "application/json"

     sendRequest c q emptyBody
     x <- receiveResponse c concatHandler
     S.putStr x
     closeConnection c

{-todo write this to file-}
