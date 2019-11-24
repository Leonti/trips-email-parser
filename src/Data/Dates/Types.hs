module Data.Dates.Types
  (DateTime (..),
   Time (..),
   months, capitalize
  ) where

import Data.Semigroup
import Data.Monoid
import Data.Char

-- | Only time, without date
data Time = 
  Time {
    tHour   :: Int,
    tMinute :: Int,
    tSecond :: Int }
  deriving (Eq,Ord)

instance Show Time where
  show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s 

data DateTime =
  DateTime {
    year  :: Int,
    month  :: Int,
    day    :: Int,
    time :: Maybe Time }
  deriving (Eq,Ord)

months :: [String]
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]

-- | capitalize first letter of the string
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

-- | Show name of given month
showMonth ::  Int -> String
showMonth i = capitalize $ months !! (i-1)

instance Show DateTime where
  show (DateTime y m d time) = 
    show d ++ " " ++ showMonth m ++ " " ++ show y ++ ", " ++
      show time