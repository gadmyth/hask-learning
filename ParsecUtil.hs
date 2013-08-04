module ParsecUtil where

import Text.Parsec.String
import Text.Parsec.Prim
import Control.Applicative hiding ((<|>))


printParse (Left l) = print "parse error: " >> print l
printParse (Right r) = print r

-- This func's equality is parseTest, another is parseFromFile
run p input = printParse $ parse p "" input


(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

-- TODO: too much case

runp head str = runp_base head str putStrLn
runp2 head str = runp_base head str putStr
runpminus head str = runp_base head str returnNull where
          returnNull _ = return ()


runparsec parsec state content = do
  case (runP parsec state "" content) of
    Left err -> do
      print err
    Right result -> do
      putStr result
      
runp_base :: Parser [String] -> String -> (String -> IO()) -> IO ()
runp_base head str printfn = do
     case (runParser head () "" str) of
          Left err -> do
               print err
          Right results -> do
                mapM_ (\x -> if x /= "" then printfn x else return ()) results

