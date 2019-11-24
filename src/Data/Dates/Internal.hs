
module Data.Dates.Internal where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void

type Parser = Parsec Void String
    
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = fmap fromIntegral $ lexeme L.decimal  

number :: Int -> Int -> Parser Int
number m1 m2 = do
  t <- integer
  case t of
    t' | t' < m1  -> fail $ "number is too small " ++ (show t') ++ " vs " ++ (show m1)
    t' | t' > m2  -> fail $ "number is too big" ++ (show t') ++ " vs " ++ (show m2)
    t' -> return t'

pYear :: Parser Int 
pYear = do
  y <- number 0 10000
  if y < 2000
    then return (y + 2000)
    else return y  

pMonth :: Parser Int
pMonth = number 1 12

pDay :: Parser Int
pDay = number 1 31