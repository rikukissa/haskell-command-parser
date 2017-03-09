module State where

import Data.List
import Data.Maybe
import Data.List.Split

import Person
import Item
import Location
import Utils

import AbsAI

data MapPoint
  = End
  | Node
    { nodeName :: String
    , north :: MapPoint
    , east :: MapPoint
    , south :: MapPoint
    , west :: MapPoint
    } deriving (Show)

data State = State
  { people :: [Person]
  , items :: [Item]
  , roomMap :: [Direction]
  } deriving (Show)


storeDirection :: State -> String -> EDirection -> String -> State
storeDirection state what direction from =
  state { roomMap = roomMap state ++ [(what, direction, from)] }

howToGo :: State -> String -> String -> String
howToGo state from to =
  maybe "don't know" (intercalate ", " . map directionName) $ getPath (roomMap state) from to

findPerson :: State -> String -> Maybe Person
findPerson state personName =
  let
    allPeople = people state
  in
    getPersonWithName personName allPeople

whereWasBefore :: State -> String -> String -> String
whereWasBefore state personName locationName =
  let
    location = Known (Position locationName)
    before =
      last' =<<
      head `fmap`
      splitOn [location] `fmap`
      locations `fmap`
      findPerson state personName
  in
    case before of
      Just (Known (Position loc)) -> loc
      Just (OneOfTwo (Position loc, Position loc2)) -> "either in " ++ loc ++ " or in " ++ loc2
      _ -> "don't know"


whereWasAfter :: State -> String -> String -> String
whereWasAfter state personName locationName =
  let
    hasFuture future =
      if null future then Nothing else Just future
    location = Known (Position locationName)
    future =
      last `fmap`
      splitOn [location] `fmap`
      locations `fmap`
      findPerson state personName

    next = head `fmap` (future >>= hasFuture)
  in
    case next of
      Just (Known (Position loc)) -> loc
      Just (OneOfTwo (Position loc, Position loc2)) -> "either in " ++ loc ++ " or in " ++ loc2
      Nothing -> "don't know"

howManyObjects :: State -> PersonName -> String
howManyObjects state personName =
  let
    allItems = items state
    allPeople = people state
    existingPerson = getPersonWithName personName allPeople
    hasItem = fmap ((==) personName . name)
    itemsHolding = fmap (\_ -> filter (fromMaybe False . hasItem . iPerson) allItems) existingPerson
    itemsLength = fmap (show . length) itemsHolding
  in
    fromMaybe "don't know" itemsLength

isPersonIn :: State -> PersonName -> LocationName -> String
isPersonIn state personName locationName =
  let
    existingPerson = getPersonWithName personName (people state)
    isThere = isPersonInLocation locationName `fmap` existingPerson
  in
    fromMaybe "maybe" isThere

whereIsItem :: State -> ItemName -> String
whereIsItem state itemName =
  let
    existingItem = find ((==) itemName . iName) (items state)
    location = fmap iLocation existingItem
  in
    case location of
      Just (OneOfTwo (Position l, Position l2)) ->
        "either in " ++ l ++ "or in" ++ l2
      Just (Known (Position l)) ->
        l
      Nothing ->
        "don't know"

updateStateByMovement :: State -> PersonName -> Location -> State
updateStateByMovement state personName newLocation =
  let
    isRightPerson = ((==) personName . name)
    existingPerson = find isRightPerson (people state)
    updatedPersonM = addLocation newLocation `fmap` existingPerson
    updatedPerson = fromMaybe (Person personName [newLocation]) updatedPersonM
  in
    updatePerson state updatedPerson

getPerson :: State -> PersonName -> Maybe Person
getPerson state personName =
  let
    isRightPerson = ((==) personName . name)
  in
    find isRightPerson (people state)

updatePerson :: State -> Person -> State
updatePerson state updatedPerson =
  let
    isRightPerson = (==) (name updatedPerson) . name
    existingPerson = find isRightPerson (people state)
    updateItem item =
      let
        needsUpdate = maybe False isRightPerson (iPerson item)
      in
        if needsUpdate then
          item
            { iPerson = Just updatedPerson
            , iLocation = (last . locations) updatedPerson
            }
        else item
  in
    case existingPerson of
      Just p ->
        state
          { people = delete p (people state) ++ [updatedPerson]
          , items = map updateItem (items state)
          }
      Nothing ->
        state { people = people state ++ [updatedPerson] }

handleMove :: State -> PersonName -> LocationName -> State
handleMove state personName locationName =
  let
    newLocation = Known (Position locationName)
  in
    updateStateByMovement state personName newLocation

handleLeave :: State -> PersonName -> LocationName -> State
handleLeave state personName locationName =
  let
    person = getPerson state personName
    personWithUpdatedLocations =
      fmap (\p -> p { locations = locations p ++ [NoLonger (Position locationName)]}) person
  in
    maybe state (updatePerson state) personWithUpdatedLocations

handleMoveToEither :: State -> PersonName -> LocationName -> LocationName -> State
handleMoveToEither state personName locationName locationName2 =
  let
    newLocation = OneOfTwo (Position locationName, Position locationName2)
  in
    updateStateByMovement state personName newLocation

handleTake :: State -> PersonName -> ItemName -> State
handleTake state personName itemName =
  let
    existingPerson = getPersonWithName personName (people state)
    existingItem = find ((==) itemName . iName) (items state)
  in
    case (existingPerson, existingItem) of
      (Just person, Just item) ->
        state { items = delete item (items state) ++ [Item itemName ((last . locations) person) existingPerson] }
      (Just person, Nothing) ->
        state { items = items state ++ [Item itemName ((last . locations) person) existingPerson] }
      _ ->
        state

handleGive :: State -> PersonName -> ItemName -> PersonName -> State
handleGive state personName itemName personName2 =
  let
    state' = handleDrop state personName itemName
    state'' = handleTake state' personName2 itemName
  in state''

handleDrop :: State -> PersonName -> ItemName -> State
handleDrop state personName itemName =
  let
    isRightPerson = (==) personName . name
    existingPerson = getPersonWithName personName (people state)
    existingItem = find ((==) itemName . iName) (items state)
    updateItem item =
      let
        needsUpdate =
          iName item == itemName &&
          maybe False isRightPerson (iPerson item)
      in
        if needsUpdate then
          item
          { iPerson = Nothing
          }
        else item
  in
    case (existingPerson, existingItem) of
      (Just person, Just item) ->
        state
        { items = map updateItem (items state)
        }
      _ ->
        state