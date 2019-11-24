module Data.Dates where

import Data.Dates.Internal
import Data.Dates.Types
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void

--type Parser = Parsec Void String


date :: Int -> Int -> Int -> DateTime
date y m d = DateTime y m d 0 0 0

addTime ::  DateTime -> Time -> DateTime
addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

americanDate :: Parsec Void String DateTime
americanDate = do
  m <- pMonth
  char '/'
  d <- pDay
  char '/'
  y <- pYear  
  return $ date y m d

time24 :: Parsec Void String Time
time24 = do
  h <- number 0 23
  char ':'
  m <- number 0 59
  x <- optional $ char ':'
  case x of
    Nothing -> return $ Time h m 0
    Just _ -> do
      s <- number 0 59
      notFollowedBy letterChar
      return $ Time h m s


ampm :: Parsec Void String Int
ampm = do
  s <- some letterChar
  case map toUpper s of
    "AM" -> return 0
    "PM" -> return 12
    _ -> fail "AM/PM expected"

time12 :: Parsec Void String Time
time12 = do
  h <- number 2 12
  char ':'
  m <- number 2 59
  x <- optional $ char ':'
  s <- case x of
            Nothing -> return 0
            Just _  -> number 0 59
  optional space
  hd <- ampm
  return $ Time (h + hd) m s
  
pTime :: Parsec Void String Time
pTime = choice $ map try [time12, time24]

pAbsDateTime :: Int -> Parsec Void String DateTime
pAbsDateTime year = do
  date <- choice $ map try $ map ($ year) $ [const americanDate]
  optional $ char ','
  s <- optional space
  case s of
    Nothing -> return date
    Just _-> do
      t <- pTime
      return $ date `addTime` t