module Location where

import Data.List
import Data.Maybe

import Utils
import AbsAI

type Direction = (String, EDirection, String)
type LocationName = String
newtype Position = Position String deriving (Eq, Show)

data Location
  = Known Position
  | OneOfTwo (Position, Position)
  | NoLonger Position
  | MaybeIn Position
  deriving (Eq, Show)

flipDirection :: EDirection -> EDirection
flipDirection EWest = EEast
flipDirection EEast = EWest
flipDirection ESouth = ENorth
flipDirection ENorth = ESouth

directionName :: EDirection -> String
directionName EWest = "west"
directionName EEast = "east"
directionName ESouth = "south"
directionName ENorth = "north"

isEntry target (from, direction, to) = from == target || to == target

getPath :: [Direction] -> String -> String -> Maybe [EDirection]
getPath [] _ _ = Nothing
getPath roomMap from to =
  let
    targets = filter (isEntry from) roomMap
    other = roomMap \\ targets
    checkPath :: Direction -> Maybe [EDirection]
    checkPath (f, d, t) =
      let
        direction = if f == from then flipDirection d else d
        nextStart = if f == from then t else f
      in
        if f == to || t == to then
          Just [direction]
        else
          (++) [direction] `fmap` getPath other nextStart to
  in

    if null targets then
      Nothing
    else
      let
        successes = filter isJust (map checkPath targets)
      in
        if null successes then Nothing else head successes