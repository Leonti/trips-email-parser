module Lib
    ( someFunc
    ) where

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
