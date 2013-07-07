{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Applicative hiding ((<|>), many)
import System.Environment (getArgs)
import ParsecUtil
import Data.Typeable

untrackhead :: Parser String
untrackhead = count 2 (char '?')
filename = many (noneOf "\"\r\n")

trackedhead :: Parser String
trackedhead = count 1 (char 'A')

modifiedhead :: Parser String
modifiedhead = count 1 (char 'M')

skipLine = do
         s <- many (noneOf "\n") <* newline
         return ""

gitStatusLine :: Parser String -> Parser String
gitStatusLine headMode = headMode *> (many1 space) *> filename <* (option "" $ many (char '\n'))

normalLine :: Parser String -> Parser String
normalLine headMode = try (gitStatusLine headMode) <|> skipLine

untracked = many $ normalLine untrackhead
tracked = many $ normalLine trackedhead
modified = many $ normalLine modifiedhead


-- TODO: too much case

runp :: Parser [String] -> String -> IO ()
runp head str = do
     case (runParser head () "" str) of
          Left err -> do
               print err
          Right files -> do
                mapM_ (\x -> if x /= "" then putStrLn x else return ()) files


dot9PNGTopNG :: Parser String
dot9PNGTopNG = (++) <$> (many (noneOf ".") <* string ".9") <*> string ".png"

deleteDot9 = many $ try dot9PNGTopNG <|> skipLine

noSplitTemplate = many space <++> string "template = " <++> skipSplit

splitstr = do
         manyTill anyChar (try (string "split_"))

skipSplit = (string "\"") <++> (splitstr <++> (many (noneOf "\""))) <++> (string "\"")

-- TODO: export the above function when add the following main

main = do
     contents <- getContents --get contents from pipeline
     args <- getArgs
     let opt = head args
     case opt of
          "-u" -> runp untracked contents
          "-a" -> runp tracked contents
          "-m" -> runp modified contents
          "-d.9" -> runp deleteDot9 contents