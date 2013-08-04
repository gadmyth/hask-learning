{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Parsec
import Text.Parsec.Prim
import System.Environment (getArgs)
import ParsecUtil

data Contact = Nil | Contact {
  cnumber :: String,
  name :: String,
  mailHead :: String,
  phone1 :: String,
  phone2 :: String,
  email :: String,
  msn :: String,
  qq :: String
  } deriving (Eq, Show)


findInQuote = do
  many (oneOf " \t\n")
  string "\""
  str <- many (noneOf "\"")
  string "\""
  return str

findInQuoteSave = try findInQuote <|> return ""

ignoreComma = do
  manyTill anyChar (try $ many $ char ',')
  return ""

ignoreLine = do
  many (oneOf " \t")
  many $ string "\n"
  return Nil

parseContact = do
  cn <- findInQuote
  ignoreComma
  nm <- findInQuote
  ignoreComma
  mh <- findInQuote
  ignoreComma
  ph1 <- findInQuote
  ignoreComma
  ph2 <- findInQuote
  ignoreComma
  em <- findInQuote
  ignoreComma
  msn <- findInQuote
  ignoreComma
  qq <- findInQuote
  ignoreComma
  ignoreLine
  return Contact {cnumber = cn, name = nm, mailHead = mh, phone1 = ph1, phone2 = ph2, email = em, msn = msn, qq = qq }

contactLine = try parseContact <|> ignoreLine

allContacts = do
  contact <- contactLine
  case contact of
    Nil -> return []
    _ -> do
      contacts <- allContacts
      return $ contact : contacts
  

main = do
  args <- getArgs
  print (head args)
  content <- readFile (head args)
  parseTest allContacts content
