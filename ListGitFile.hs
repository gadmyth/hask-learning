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

origLine = many (noneOf "\n") <++> (count 1 newline)

gitStatusLine :: Parser String -> Parser String
gitStatusLine headMode = headMode *> (many1 space) *> filename <* (option "" $ many (char '\n'))

normalLine :: Parser String -> Parser String
normalLine headMode = try (gitStatusLine headMode) <|> skipLine

untracked = many $ normalLine untrackhead
tracked = many $ normalLine trackedhead
modified = many $ normalLine modifiedhead


-- TODO: too much case

runp head str = runp_base head str putStrLn
runp2 head str = runp_base head str putStr
runpminus head str = runp_base head str returnNull where
          returnNull _ = return ()


runp_base :: Parser [String] -> String -> (String -> IO()) -> IO ()
runp_base head str printfn = do
     case (runParser head () "" str) of
          Left err -> do
               print err
          Right results -> do
                mapM_ (\x -> if x /= "" then printfn x else return ()) results


dot9PNGTopNG :: Parser String
dot9PNGTopNG = (++) <$> (many (noneOf ".") <* string ".9") <*> string ".png"

deleteDot9 = many $ try dot9PNGTopNG <|> skipLine

nocurvetplline = many (oneOf " \t") <++> string "template = " <++> skipcurveinquote
noCurveTemplateLine = many $ try nocurvetplline <|> try altnocurveline <|> try shiftnocurveline <|> origLine

altnocurveline = many (oneOf " \t") <++> string "alt_inputs = " <++> skipcurve
shiftnocurveline = many (oneOf " \t") <++> string "shift_inputs = " <++> tobenil

tobenil = do
        many (noneOf "\n")
        return "nil"

findstr str = do
         manyTill (letter <|> oneOf "_.") (try (string str))

skipcurveinquote = (string "\"") <++> skipcurve <++> (string "\"")

skipcurve = (findstr "_curve") <++> (many (noneOf "\"\n")) <++> (count 1 newline)


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
          "-nc" -> runp2 noCurveTemplateLine contents