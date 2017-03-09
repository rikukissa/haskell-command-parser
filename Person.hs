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
    prevLocations = lastN 3 $ locations person
    either l l2 =
      if l == location || l2 == location then
        "maybe"
      else "no"
  in
    case prevLocations of
      [(OneOfTwo (l, l2)), (Known loc2), (NoLonger loc3)] ->
        if loc2 == loc3 && (l == location || l2 == location) then
          "maybe"
        else
          "no"
      [(Known loc), (Known loc2), (NoLonger loc3)] ->
        if loc2 == loc3 && loc == location then
          "maybe"
        else
          "no"
      history -> case (last' history) of
        Just (OneOfTwo (l, l2)) -> either l l2
        Just (Known l) -> boolToAnswer $ l == location
        _ -> "no"


