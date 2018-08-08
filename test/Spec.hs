module Main where

import Data.Semigroup
import Data.Text(Text, pack)
import Distribution.Simple.Command
import Distribution.ReadE(ReadE(ReadE), succeedReadE)
import Distribution.Simple.Setup (Flag(Flag,NoFlag), flagToMaybe,toFlag)
import Numeric (readDec)
import System.Environment (getArgs)
import TextShow

import EasyTest

import TestCounter

data LEIATestFlags =
  LEIATestFlags
    { leiaTestScope :: Flag Text
    , leiaTestSeed :: Flag Integer
    }
  deriving Show

instance Semigroup LEIATestFlags where
  (<>) a b = LEIATestFlags
    { leiaTestScope = mult leiaTestScope
    , leiaTestSeed = mult leiaTestSeed
    }
    where
      mult :: (LEIATestFlags -> Flag b) -> (Flag b)
      mult f = (f a) <> (f b)

instance Monoid LEIATestFlags where
  mempty = LEIATestFlags
    { leiaTestScope = mempty
    , leiaTestSeed = mempty
    }
  mappend = (<>)

runReadNumE :: (Num numType, Eq numType) => String -> Either String (Flag numType)
runReadNumE s =
  case readDec s of
    [(value, "")] -> (Right $ Flag value)
    _ -> (Left $ "Not an integer: '" ++ s ++ "'")

toStrings :: (Show a) => Flag a -> [String]
toStrings flag =
  case flagToMaybe flag of
    Nothing -> []
    (Just intValue) -> [show intValue]

main :: IO ()
main =
  do let defaultFlags = LEIATestFlags NoFlag NoFlag
         scopeOptionField =
           OptionField
             "scope"
             [reqArg
               "scope"
               (succeedReadE (toFlag . pack))
               (\_ -> [])
               ['s']
               ["scope"]
               "The scope that limits the tests to run"
               leiaTestScope
               (\flag flags -> flags { leiaTestScope = flag })
             ]
         randomSeedField =
           OptionField
             "random-seed"
             [reqArg
               "seed"
               (ReadE runReadNumE)
               toStrings
               ['r']
               ["random-seed", "seed"]
               "The random seed for the tests"
               leiaTestSeed
               (\flag flags -> flags { leiaTestSeed = flag })
             ]
         leiaTestCLI = mkCommandUI
           "leiaTest"
           "LEIA: Learning Expectations Incrementally Autonomously (Test)"
           ["[ --scope <test-scope> ] [ --seed <random-generator-seed> ]"]
           defaultFlags
           (\_ ->
             [ scopeOptionField
             , randomSeedField
             ]
           )
         runCommand = commandAddAction leiaTestCLI (\_ _ -> ())
     putStrLn ""
     args <- getArgs
     printT ["Command line arguments", showt args]
     putStrLn $ commandUsage leiaTestCLI ""
     commandFlags <- return $ commandsRun leiaTestCLI [runCommand] args
     flags <- do
       case commandFlags of
         (CommandReadyToGo (theFlags, _)) ->
           do printT ["Command ready to go", show theFlags]
              return $ theFlags
         _ ->
           do putStrLn $ commandUsage leiaTestCLI ""
              error "Exit"
     printT ["Random seed", show $ leiaTestSeed flags]
     case (leiaTestScope flags, leiaTestSeed flags) of
       (NoFlag, NoFlag) -> run testCounter
       (NoFlag, Flag(seed)) -> rerun (fromIntegral seed) testCounter
       (Flag(testScope), NoFlag) -> runOnly testScope testCounter
       (Flag(testScope), Flag(seed)) -> rerunOnly (fromIntegral seed) testScope testCounter
