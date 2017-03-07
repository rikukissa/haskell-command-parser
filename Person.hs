module Person where

import Data.List

import Location
import Utils


type PersonName = String

data Person = Person
  { name :: PersonName
  , locations :: [Location]
  } deriving (Show, Eq)


-- getPersonWithName :: S -> String -> Maybe Person
getPersonWithName personName = find (((==) personName) . name)

addLocation location person =
  person
  { locations = (locations person) ++ [location]
  }

isPersonInLocation :: String -> Person -> String
isPersonInLocation locationName person =
  let
    location = (Position locationName)
    prevLocations = filterOutCancelling $ locations person
    prevLocation = last prevLocations
    either l l2 =
      if l == location || l2 == location then
        "maybe"
      else "no"
  in
    case prevLocation of
      OneOfTwo (l, l2) -> either l l2
      Known l -> boolToAnswer $ l == location
      _ -> "no"