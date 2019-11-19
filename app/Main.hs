module Main where

import Lib

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

test :: IO ()
test = parseTest (satisfy (== 'a') :: Parser Char) ""

-- parseTest (string' "foo" :: Parser String) "FOO"

mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

-- https://markkarpov.com/megaparsec/megaparsec.html#white-space
-- https://www.reddit.com/r/haskell/comments/7jbfuo/megaparsec_and_lexers_im_missing_a_concept/

main :: IO ()
main = do
  content <- readFile "rawText.txt"
  putStrLn content
