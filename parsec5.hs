{-# LANGUAGE NoMonomorphismRestriction #-}

-- | http://ocharles.org.uk/blog/posts/2012-12-10-24-days-of-hackage-parsec.html

import Text.Parsec
import Text.Parsec.Prim
import Control.Applicative hiding ((<|>))
import Data.Monoid (mconcat)


countryCode = count 2 upper

regCode = count 3 upperNum
        where upperNum = upper <|> digit

regYear = count 2 digit

recordingID = count 5 digit

data ISRC = ISRC {
     iCountryCode :: String,
     iRegCode :: String,
     iRegYear :: Int,
     iRecoding :: Int
} deriving (Show)

isrcParser = ISRC <$> countryCode <*> regCode <*> fmap read regYear <*> fmap read recordingID <* eof

-- parseTest isrcParser "USPR37300012"

isrcParser2 = mconcat <$> sequence [countryCode, regCode, regYear, recordingID] <* eof