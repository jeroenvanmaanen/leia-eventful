module Counter where

import Eventful

newtype Counter = Counter { unCounter :: Int }
  deriving (Show, Eq)

incrementCounter :: Counter -> Int -> Counter
incrementCounter (Counter count) amount = Counter (count + amount)

decrementCounter :: Counter -> Int -> Counter
decrementCounter (Counter count) amount = Counter (count - amount)

resetCounter :: Counter -> Counter
resetCounter _ = Counter 0

data CounterEvent
  = CounterIncremented Int
  | CounterDecremented Int
  | CounterReset
  deriving (Show, Eq)

myEvents :: [CounterEvent]
myEvents =
  [ CounterIncremented 3
  , CounterDecremented 1
  , CounterReset
  ]

handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent counter (CounterIncremented amount) = incrementCounter counter amount
handleCounterEvent counter (CounterDecremented amount) = decrementCounter counter amount
handleCounterEvent counter (CounterReset) = resetCounter counter

counterProjection :: Projection Counter CounterEvent
counterProjection =
  Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleCounterEvent
  }

myLatestCounter :: Counter
myLatestCounter = latestProjection counterProjection myEvents
-- Counter {unCounter = 0}

allMyCounters :: [Counter]
allMyCounters = allProjections counterProjection myEvents
-- [ Counter {unCounter = 0}
-- , Counter {unCounter = 3}
-- , Counter {unCounter = 2}
-- , Counter {unCounter = 0}
-- ]
