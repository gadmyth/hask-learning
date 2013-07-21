-- | http://www.vex.net/~trebla/haskell/parsec-generally.xhtml

import Text.Parsec
import Text.Parsec.String


pmain :: Parser Integer
pmain = do
  x <- pnum `chainl1` pplus
  eof
  return x

pnum = read `fmap` many1 digit

pplus = char '+' >> return (+)

-- parseTest pmain "1+2+3"
