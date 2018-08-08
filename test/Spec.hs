module Main where

import Distribution.Simple.Command
import EasyTest
import TestCounter

main :: IO ()
main =
  do
    putStrLn ""
    runOnly "Counter" testCounter
