module Item where

import Person
import Location

type ItemName = String
data Item = Item
  { iName :: ItemName
  , iLocation :: Location
  , iPerson :: Maybe Person
  } deriving (Show, Eq)