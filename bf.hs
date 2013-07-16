import Text.Parsec
import Text.Parsec.String

data BFInstruction = GoBack | GoForward | Increment | Decrement | Input
	 			   | Output | Loop [BFInstruction]
				   deriving (Show)


parseGen x y = char x >> return y
parseBack = parseGen '<' GoBack
parseForward = parseGen '>'  GoForward
parseIncrement = parseGen '+' Increment
parseDecrement = parseGen '-' Decrement
parseInput = parseGen ',' Input
parseOutput = parseGen '.' Output

parseLoop = do
  char '['
  insn <- parseInstructions
  char ']'
  return $ Loop insn
             
parseComment = do
  many $ noneOf "<>+-,.[]"
  return ()

parseInstruction = do
  parseComment
  i <- parseBack <|> parseForward <|> parseIncrement <|> parseDecrement <|> parseInput <|> parseOutput <|> parseLoop
  parseComment
  return i

parseInstructions :: Parser [BFInstruction]
parseInstructions = many parseInstruction


main :: IO ()
main = do
  cont <- readFile "hello.bf"
  case parse parseInstructions "hello.bf" cont of
    Left e -> print e
    Right insn -> print insn

