-- | https://www.fpcomplete.com/user/adinapoli/the-pragmatic-haskeller/episode-5-a-simple-dsl

import Text.Parsec.Char (letter, oneOf, digit, char, noneOf, string, newline)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse, many, try)
import Text.Parsec.Combinator (many1, optionMaybe)
import Control.Applicative hiding (optional, many)

import PPTypes

printParse (Left l) = print "parse error: " >> print l
printParse (Right r) = print r

run p input = printParse $ parse p "" input

ws :: Parser String
ws = many (oneOf " \t")

word :: Parser String
word = many letter

trim :: Parser a -> Parser a
trim p = ws *> p <* ws

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit

stringLike :: Parser String
stringLike = char '"' *> many (noneOf "\"\r\n") <* char '"'

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

measureP :: Parser (Maybe String)
measureP = (string "gr" *> (pure . Just $ "gr"))
         <|> (string "ml" *> (pure . Just $ "ml"))
         <|> (string "spoon" *> (pure . Just $ "spoon"))
         <|> (string "cut" *> (pure . Just $ "cut"))
         <|> (pure Nothing)

syntactcSugar :: String -> Parser (Maybe String)
syntactcSugar s = (string s *> (pure . Just $ s)) <|> pure Nothing

ingredient :: Parser Ingredient
ingredient = do
           qt <- trim int
           ms <- trim measureP
           trim (syntactcSugar "of")
           name <- trim stringLike
           trim (syntactcSugar "and")
           string "\n"
           return $ Ingredient name qt ms

step :: Parser Step
step = do
     sn <- trim stringLike
     d <- optionMaybe durationP
     trim (syntactcSugar "and")
     string "\n" <||> pure ""
     return $ Step sn 1 d

durationP :: Parser Duration
durationP = do
          trim (string "for")
          d <- trim int
          u <- trim durationUnit
          return $ Duration d u
          where durationUnit = string "seconds" <|> string "minutes" <|> string "hours"

recipe :: Parser Recipe
recipe = do
       rn <- trim stringLike
       trim (syntactcSugar "is made with") *> string "\n"
       i <- many1 ingredient
       many newline
       trim (string "prepared by") *> string "\n"
       s <- many1 step
       return $ Recipe rn i s


main :: IO ()
main = do
     str <- readFile "dsl"
     print str
     print $ parse recipe "" str