-- | http://legacy.cs.uu.nl/daan/download/parsec/parsec.html
-- parsec-3.1.3
-- import Text.Parsec
-- import Text.Parsec.Prim

-- parsec-2.1.0.1
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim

simple :: Parser Char
simple = letter

printParse (Left l) = print "parse error: " >> print l
printParse (Right r) = print r

-- | example: run simple "abc"
run :: Show a => Parser a -> String -> IO ()
run p input = printParse $ parse p "" input

openClose :: Parser Char
openClose = char '(' >> char ')'

parens :: Parser ()
parens = (char '(' >> parens >> char ')' >> parens) <|> return ()

testOr1 = char '(' >> (char 'a' <|> char 'b') >> char ')'

testOr2 = try (string "(a)") <|> string "(b)"

word = letter >>= (\c -> (word >>= (\cs -> return (c:cs))) <|> return [c])

sentence = sepBy word (skipMany1 (oneOf ",.?!" <|> space))
