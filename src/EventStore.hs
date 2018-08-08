module EventStore where

import Control.Concurrent.STM
import Eventful
import Eventful.Store.Memory

import Counter

counterStoreExample :: IO ()
counterStoreExample = do
  -- First we need to create our in-memory event store.
  tvar <- eventMapTVar
  let
    writer = tvarEventStoreWriter tvar
    reader = tvarEventStoreReader tvar

  -- Lets store some events. Note that the 'atomically' functions is how we
  -- execute STM actions.
  let
    uuid = read "123e4567-e89b-12d3-a456-426655440000"
    events =
      [ CounterIncremented 3
      , CounterDecremented 1
      , CounterReset
      ]
  _ <- atomically $ storeEvents writer AnyVersion uuid events

  -- Now read the events back and print
  events' <- atomically $ getEvents reader (allEvents uuid)
  print events'

  _ <- atomically $ storeEvents writer AnyVersion uuid [ CounterIncremented 2 ]
  events'' <- atomically $ getEvents reader (allEvents uuid)
  print events''
