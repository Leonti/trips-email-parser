module Main where

import Lib

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import Replace.Megaparsec

import Data.Either

import Data.Dates.Internal(pMonth, number)
import Data.Dates
import Data.Dates.Types(DateTime, Time)

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
    [ string' "Sydney"
    , string' "Melbourne"
    , string' "Taoyuan" ]
  o <- getOffset   
  return (m, o)

withOffsetDateTime :: Parser (DateTime, Int)
withOffsetDateTime = do
  m <- (pAbsDateTime 2019)
  o <- getOffset   
  return (m, o)
 
withOffsetTime :: Parser (Time, Int)
withOffsetTime = do
  m <- pTime
  o <- getOffset   
  return (m, o)    

testOffset :: IO ()
testOffset = parseTest (return . rights =<< sepCap withOffset) "hello I'm going to Moscow and Melbourne later"

testOffsetDateTime :: IO ()
testOffsetDateTime = parseTest (return . rights =<< sepCap withOffsetDateTime) "hello 11/18/2019 11:00pm how are"


testTime :: String -> IO ()
testTime = parseTest pTime

testAbs :: String -> IO ()
testAbs = parseTest (pAbsDateTime 2019)

tMonth :: IO ()
tMonth = parseTest pMonth "11"
-- Sunday 5 January 2020

--testDate :: IO ()


hello :: Parser String
hello = string "hello"

fresh :: Parser String
fresh = string "fresh"

helloFresh :: Parser String
helloFresh = do
  hello
  s <- optional $ some spaceChar
  case s of
    Nothing -> return ""
    Just _ -> fresh

mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

-- https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/switch-from-parsec-to-megaparsec.md
-- https://markkarpov.com/megaparsec/megaparsec.html#white-space
-- https://www.reddit.com/r/haskell/comments/7jbfuo/megaparsec_and_lexers_im_missing_a_concept/
-- https://github.com/portnov/dates/blob/master/Data/Dates.hs

main :: IO ()
main = do
  content <- readFile "rawText.txt"
  parseTest (return . rights =<< sepCap withOffsetDateTime) content
  parseTest (return . rights =<< sepCap withOffsetTime) content
  parseTest (return . rights =<< sepCap withOffset) content
  putStrLn "done!"
--  putStrLn content
