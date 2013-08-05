{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main,
	   skipLine,
	   origLine,
	   gitStatusLine,
	   normalLine,
	   modifiedhead,
	   trackedhead,
	   untrackhead,
	   ignoreendline,
	   untracked,
	   tracked,
	   modified) where

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
modifiedhead = many (char ' ') >> count 1 (char 'M')

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


dot9PNGTopNG :: Parser String
dot9PNGTopNG = (++) <$> (many (noneOf ".") <* string ".9") <*> string ".png"

appendDot9PNG str = appendName ".9.png" str

appendName suffix str = do
             s1 <- many (noneOf ".")
             s2 <- string suffix
             return $ s1 ++ str ++ s2

appendDot9 str = many $ try (appendDot9PNG str) <|> skipLine

appendxml str = many $ try (appendName ".xml" str) <|> skipLine
             
deleteDot9 = many $ try dot9PNGTopNG <|> skipLine


nocurvetplline = many (oneOf " \t") <++> string "template = " <++> skipcurveinquote
noCurveTemplateLine = many $ try nocurvetplline <|> try altnocurveline <|> try shiftnocurveline <|> origLine

altnocurveline = many (oneOf " \t") <++> string "alt_inputs = " <++> skipcurve
shiftnocurveline = many (oneOf " \t") <++> string "shift_inputs = " <++> tobenil

tobenil = do
        many (noneOf "\n")
        return "nil"

-- | Tool
find_skip_str str = do
         manyTill anyChar (try (string str))

find_skip_str' not_str str = do
  manyTill (noneOf not_str) (try (string str))
-- | Tool
findstr str = do
  x <- find_skip_str str 
  return $ x ++ str

findstr' not_str str = do
  x <- find_skip_str' not_str str
  return $ x ++ str

-- |usage: 
-- |runP findSplit False "" "<Key keyName=\"sk_split\" keyIcon=\"@drawable/key_fore_split\" backgroundType=\"fun\" mainOnlyTextSize=\"@dimen/button_textsize\"/>"
findSplit = do
  x <- findstr "\"sk_split\""
  y <- try (findstr' ">" "ignoreCurve=\"true\"" <++> find_skip_str ">") <|> do {setState True; find_skip_str ">"}
  z <- many anyChar
  st <- getState
  if st then return $ x ++ " ignoreCurve=\"true\"" ++ y ++ ">" ++ z else return $ x ++ y ++ ">" ++ z

findSplitAll = try findSplit <|> many anyChar

skipPreview = try skipsymspreview <|> many anyChar
skipsymspreview = do
  x <- findstr "\"sk_sym\""
  y <- try (find_skip_str' ">" "iconPreview=\"@drawable/key_preview_sym\"" <++> find_skip_str' ">" "iconPreviewRight=\"@drawable/key_fore_sym_emo\"") <|> findstr ">"
  z <- many anyChar
  return $ x ++ y ++ z

skipsymchs = try skipchssym <|> many anyChar
skipchssym = do
  x <- findstr "s.softkeys[\"sk_sym\"] = ck.set_sym_key(\"eng_sym_\", \"split_\""
  y <- try (find_skip_str ", true") <|> many anyChar
  z <- many anyChar
  return $ x ++ y ++ z
 
skipcurveinquote = (string "\"") <++> skipcurve <++> (string "\"")

skipcurve = (find_skip_str "_curve") <++> (many (noneOf "\"\n")) <++> (count 1 newline)

insertNL content = do
         s <- count 1 newline
         return $ s ++ content ++ "\n"
         
insertsplitsettingline = do
                       head <- many (oneOf " \t")
                       s1  <- string "si.change_surface_by_id"
                       s2 <- many (noneOf "\n")
                       s3 <- insertNL $ head ++ "ck.changeSplitSetting()"
                       return $ head ++ s1 ++ s2 ++ s3


insertSurface = many $ try (insertInBracket "ck.changeSplitSetting" "s") <|> origLine
insertInBracket head str = do
                s <- many (oneOf " \t")
                s0 <- string head
                s1 <- many (noneOf "(")
                bl <- count 1 (char '(')
                br <- count 1 (char ')')
                s2 <- count 1 newline
                return $ s ++ s0 ++ s1 ++ bl ++ str ++ br ++ s2

insertSSL = many $ try insertsplitsettingline <|> origLine

deleteLines start linenum = many $ try (linehead start linenum) <|> origLine
deletedisablefirstalt = deleteLines "--disable slide_down for 1st" 13

appendDrawableState = do
                    s1 <- many (oneOf " \t")
                    s2 <- string "<item "
                    s3 <- string "android:drawable=\""
                    s4 <- many (noneOf "\"")
                    s5 <- many (noneOf "\n")
                    s6 <- count 1 newline
                    return $ s1 ++ s2 ++ "android:state_selected=\"true\" " ++ s3 ++ s4 ++ "_scale" ++ s5 ++ s6
                           ++ s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6

modifydrawablename = do
                    s1 <- many (oneOf " \t")
                    s2 <- string "<item "
                    s3 <- string "android:drawable=\""
                    s4 <- many (noneOf "\"")
                    s5 <- many (noneOf "\n")
                    s6 <- count 1 newline
                    return $ s1 ++ s2 ++ s3 ++ s4 ++ "_scale" ++ s5 ++ s6

modifyDN = many $ try modifydrawablename <|> origLine
                           
appendDS = many $ try appendDrawableState <|> origLine

linehead str linenum = do
         string str
         count linenum skipLine
         return ""

ignoreendline = string "end" <* count 2 newline <* string "end"
ignoreEND = many $ try ignoreendline <|> origLine

enableOrder = many $ try enableorderline <|> origLine
enableorderline = do
                s1 <- many (noneOf "(") 
                s2 <- many (noneOf ",")
                s3 <- string ", \"split_\""
                s4 <- string ")\n"
                return $ s1 ++ s2 ++ s3 ++ ", true" ++ s4


type PParser = Parsec String Bool
tak :: PParser String
tak = do
  s <- many (oneOf " \t")
  st <- getState
  setState (not st)
  s2 <- string "fuck"
  s3 <- try (string "end") <|> string "eof"
  return $ s ++ s2 ++ s3 ++ (show st)

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
          "-a.9" -> runp2 (appendDot9 "_scale") contents
          "-axml" -> runp2 (appendxml "_scale") contents
          "-nc" -> runp2 noCurveTemplateLine contents
          "-iss" -> runp2 insertSSL contents
          "-ibs" -> runp2 insertSurface contents
          "-skip" -> runp2 deletedisablefirstalt contents
          "-ads" -> runp2 appendDS contents
          "-mdn" -> runp2 modifyDN contents
          "-igend" -> runp2 ignoreEND contents
          "-eo" -> runp2 enableOrder contents
          "-fs" -> runparsec findSplitAll False contents
          "-ssp" -> runparsec skipPreview False contents
          "-scs" -> runparsec skipsymchs False contents
