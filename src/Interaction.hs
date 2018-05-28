module Interaction where
import Data.HashMap.Strict(HashMap)

data Category = Category String

data Item = Item Category String

newtype Context = Context [Item]

newtype Counts = Counts (HashMap Item Integer)
newtype Bounds = Bounds (HashMap Item Rational)
