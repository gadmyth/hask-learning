{-# LANGUAGE NoMonomorphismRestriction #-}

-- | https://www.fpcomplete.com/user/stevely/parsing-floats-with-parsec

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Applicative hiding ((<|>))

import ParsecUtil

number = many1 digit

plus = char '+' *> number

minus = (:) <$> char '-' <*> number --this can concat what read

integer = plus <|> minus <|> number

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

float = integer <++> decimal <++> exponent where
      decimal = option "" $ char '.' <:> number
      exponent = option "" $ oneOf "eE" <:> number

readfloat = rd <$> float where
      rd = read :: String -> Float