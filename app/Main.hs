module Main where

import Lib

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

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

main :: IO ()
main = do
  content <- readFile "rawText.txt"
  putStrLn content
