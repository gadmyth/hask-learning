module ParsecUtil where

import Text.Parsec.Prim
import Control.Applicative hiding ((<|>))


printParse (Left l) = print "parse error: " >> print l
printParse (Right r) = print r

-- This func's equality is parseTest, another is parseFromFile
run p input = printParse $ parse p "" input


(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

