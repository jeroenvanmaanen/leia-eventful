module Main where

import EasyTest
import TestCounter

main :: IO ()
main =
  do
    putStrLn ""
    runOnly "Counter" testCounter
