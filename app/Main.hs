module Main where

import Lib

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import Replace.Megaparsec

import Data.Either

import Data.Dates.Internal(pMonth)
import Data.Dates

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

location :: Parser String
location = choice 
  [ string' "Moscow"
  , string' "Melbourne" ]

test :: IO ()
--test = parseTest (satisfy (== 'a') :: Parser Char) ""
test = parseTest location "hello I'm going to Moscow later"

hexparser = chunk "0x" >> L.hexadecimal :: Parsec Void String Integer

testReplace :: IO ()
testReplace = parseTest (sepCap hexparser) "0xA 000 0xFFFF"

spaceoffset :: Parsec Void String Int
spaceoffset = getOffset <* space1 :: Parsec Void String Int

withOffset :: Parser (String, Int)
withOffset = do
  m <- choice 
    [ string' "Moscow"
    , string' "Melbourne" ]
  o <- getOffset   
  return (m, o)

testOffset :: IO ()
testOffset = parseTest (return . rights =<< sepCap withOffset) "hello I'm going to Moscow and Melbourne later"

testMonth :: IO ()
testMonth = parseTest (return . rights =<< sepCap pMonth) "hello 6 44 12 04"


testAmerican :: IO ()
testAmerican = parseTest americanDate "4/18/2019"

testTime :: String -> IO ()
testTime = parseTest pTime

testAbs :: String -> IO ()
testAbs = parseTest (pAbsDateTime 2019)

tMonth :: IO ()
tMonth = parseTest pMonth "11"
-- Sunday 5 January 2020

--testDate :: IO ()


mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

-- https://markkarpov.com/megaparsec/megaparsec.html#white-space
-- https://www.reddit.com/r/haskell/comments/7jbfuo/megaparsec_and_lexers_im_missing_a_concept/
-- https://github.com/portnov/dates/blob/master/Data/Dates.hs

main :: IO ()
main = do
  content <- readFile "rawText.txt"
  putStrLn content
