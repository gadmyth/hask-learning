{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import System.IO

main = do
     content <- LBS.readFile "persons.json"
     System.IO.print $ (decode content :: Maybe Value)