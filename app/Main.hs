module Main where

import Lib

main :: IO ()
main = do
  content <- readFile "rawText.txt"
  putStrLn content
