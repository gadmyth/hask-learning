import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Prim (parse, many, (<|>), lookAhead, try, runParser, skipMany)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, combine)


main :: IO ()
main = do
     args <- getArgs
     let input = head args
         --output = combine (takeDirectory input) "result.sur.png"
         output = input
     str <- readFile input
     case (runParser addCommaBeforeSymKeyLine () "" str) of
          Left err -> do
               putStr "parse error at "
               print err
          Right x -> writeFile output x
     return ()
     


simple :: Parser Char
simple = letter

printParse (Left l) = print "parse error: " >> print l
printParse (Right r) = print r

run p input = printParse $ parse p "" input

addCommaBeforeSymKeyLine :: Parser String
addCommaBeforeSymKeyLine = do
              s <- try (string "s.softkeys[\"sk_sym\"] = ck.set_sym_key")
              s1 <- try (many (noneOf "\n"))
              s2 <- try (many newline)
              do
                ns <- addCommaBeforeSymKeyLine
                return ("s.softkeys[\"Comma\"] = ck.sk_comma_eng\n" ++ s ++ s1 ++ s2 ++ ns)
      <|> lookAhead (eof >> return "")
      <|> do
          s <- many (noneOf "\n")
          newline
          do
             ns <- addCommaBeforeSymKeyLine
             return (s ++ "\n" ++ ns)


findDot :: Parser String
findDot = do
              s0 <- many (oneOf "\t ")
              s <- string "<Key keyName=\"Dot\""
              s1 <- many (noneOf "\n")
              s2 <- many newline
              do
                ns <- addComma
                return (s0 ++ "<Key keyName=\"Comma\"/>\n" ++ s0 ++ s ++ s1 ++ s2 ++ ns) 

addComma :: Parser String
addComma = do
         s <- try findDot
         return s
      <|> lookAhead (eof >> return "")
      <|> do
          s <- many (noneOf "\n")
          newline
          do
             ns <- addComma
             return (s ++ "\n" ++ ns)



findDotWidth :: Parser String
findDotWidth = do
             s0 <- many (oneOf "\t ")
             s <- (string "<Key keyIcon=\"@drawable/key_fore_shift\" keyName=\"sk_sft\""
               <|> string "<CreateWordKey"
               <|> string "<SeparatorKey")
             s1 <- many (noneOf "\n")
             s2 <- many newline
             s3 <- many (oneOf "\t ")
             string "keyWidth="
             skipMany (noneOf " ")
             s4 <- many (noneOf "\n")
             s5 <- many newline
             do
                ns <- deleteDotWidth
                return (s0 ++ s ++ s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ ns)
             
deleteDotWidth :: Parser String
deleteDotWidth = do
               s <- try findDotWidth
               return s
       <|> lookAhead (eof >> return "")
       <|> do
           s <- many (noneOf "\n")
           newline
           do
           ns <- deleteDotWidth
           return (s ++ "\n" ++ ns)