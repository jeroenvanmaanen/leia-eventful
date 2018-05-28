module Events where
import Interaction
import Data.HashMap.Strict(HashMap)

data LeiaEvent
  = ObservedEvent Context Item
  | ObservedSnapshotEvent (HashMap Context Counts)
