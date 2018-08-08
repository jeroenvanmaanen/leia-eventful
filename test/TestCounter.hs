module TestCounter (testCounter) where

import EasyTest
import Counter

testCounter :: Test ()
testCounter = tests
  [ scope "Counter.increment" $ expect (unCounter (incrementCounter (Counter 1) 2) == 3)
  , scope "Counter.handleCounterEvent.incremented" $
      expect (unCounter (handleCounterEvent (Counter 1) (Counter.CounterIncremented 2)) == 3)
  , scope "Counter.dummy" $
      do
        io $ putStrLn "Hello there!"
        expect True
  ]
