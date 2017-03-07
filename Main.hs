module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import LexAI
import ParAI
import AbsAI
import ErrM

type PersonName = String
type ItemName = String
type LocationName = String

-- Todo get from Control.Lense
(<&>) = flip fmap

newtype Position = Position String deriving (Eq, Show)

data PersonLocation
  = Known Position
  | OneOfTwo (Position, Position)
  | NoLonger Position
  deriving (Eq, Show)

data Person = Person
  { name :: PersonName
  , locations :: [PersonLocation]
  } deriving (Show, Eq)

data Item = Item
  { iName :: ItemName
  , iLocation :: PersonLocation
  , iPerson :: Maybe Person
  } deriving (Show, Eq)

data State = State
  { people :: [Person]
  , items :: [Item]
  } deriving (Show)

getPersonWithName personName = find (((==) personName) . name)

findPerson :: State -> String -> Maybe Person
findPerson state personName =
  let
    allPeople = people state
  in
    getPersonWithName personName allPeople

chunk' :: [a] -> [[a]]
chunk' [] = []
chunk' [x] = [[x]]
chunk' [x,y] = [[x, y]]
chunk' (x:y:xs) =
  [[x, y]] ++ (chunk' xs)

whereWasBefore :: State -> String -> String -> String
whereWasBefore state personName locationName =
  let
    location = (Known (Position locationName))
    before =
      (findPerson state personName) <&>
      locations <&>
      (splitOn [location]) <&>
      head <&>
      last
  in
    case before of
      Just (Known (Position loc)) -> loc
      Just (OneOfTwo ((Position loc), (Position loc2))) -> "either in " ++ loc ++ " or in " ++ loc2
      Nothing -> "don't know"


whereWasAfter :: State -> String -> String -> String
whereWasAfter state personName locationName =
  let
    hasFuture future =
      if (length future) == 0 then Nothing else (Just future)
    location = (Known (Position locationName))
    future =
      (findPerson state personName) <&>
      locations <&>
      (splitOn [location]) <&>
      last

    next = (future >>= hasFuture) <&> head
  in
    case next of
      Just (Known (Position loc)) -> loc
      Just (OneOfTwo ((Position loc), (Position loc2))) -> "either in " ++ loc ++ " or in " ++ loc2
      Nothing -> "don't know"

howManyObjects :: State -> PersonName -> String
howManyObjects state personName =
  let
    allItems = items state
    allPeople = people state
    existingPerson = getPersonWithName personName allPeople
    hasItem = fmap (((==) personName) . name)
    itemsHolding = fmap (\_ -> filter ((fromMaybe False) . hasItem . iPerson) allItems) existingPerson
    itemsLength = fmap (show . length) itemsHolding
  in
    fromMaybe "don't know" itemsLength

boolToAnswer b = if b then "yes" else "no"

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

isCancelling [(Known loc), (NoLonger loc2)] = loc == loc2
isCancelling _ = False

filterOutCancelling :: [PersonLocation] -> [PersonLocation]
filterOutCancelling locations =
  reverse $
  concat $
  (not . isCancelling . reverse) `filter`
  (chunk' (reverse locations))

isPersonLocation :: String -> Person -> String
isPersonLocation locationName person =
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

isPersonIn :: State -> PersonName -> LocationName -> String
isPersonIn state personName locationName =
  let
    existingPerson = getPersonWithName personName (people state)
    isThere = existingPerson <&> (isPersonLocation locationName)
  in
    fromMaybe "maybe" isThere

whereIs :: State -> ItemName -> String
whereIs state itemName =
  let
    existingItem = find (((==) itemName) . iName) (items state)
    location = fmap iLocation existingItem
  in
    case location of
      Just (OneOfTwo ((Position l), (Position l2))) ->
        "either in " ++ l ++ "or in" ++ l2
      Just (Known (Position l)) ->
        l
      Nothing ->
        "don't know"

addLocation location person =
  person
  { locations = (locations person) ++ [location]
  }

updateStateByMovement :: State -> PersonName -> PersonLocation -> State
updateStateByMovement state personName newLocation =
  let
    isRightPerson = (((==) personName) . name)
    existingPerson = find isRightPerson (people state)
    updatedPersonM = existingPerson <&> (addLocation newLocation)
    updatedPerson = fromMaybe (Person personName [newLocation]) updatedPersonM
  in
    updatePerson state updatedPerson

getPerson :: State -> PersonName -> Maybe Person
getPerson state personName =
  let
    isRightPerson = (((==) personName) . name)
  in
    find isRightPerson (people state)

updatePerson :: State -> Person -> State
updatePerson state updatedPerson =
  let
    isRightPerson = (((==) (name updatedPerson)) . name)
    existingPerson = find isRightPerson (people state)
    updateItem item =
      let
        needsUpdate = fromMaybe False (fmap isRightPerson (iPerson item))
      in
        if needsUpdate then
          item
          { iPerson = Just updatedPerson
          , iLocation = ((last . locations) updatedPerson)
          }
        else item
  in
    case existingPerson of
      Just p ->
        state
          { people = (delete p (people state)) ++ [updatedPerson]
          , items = map updateItem (items state)
          }
      Nothing ->
        state { people = (people state) ++ [updatedPerson] }

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
      fmap (\p -> p { locations = (locations p) ++ [(NoLonger (Position locationName))]}) person
  in
    fromMaybe state (fmap (updatePerson state) personWithUpdatedLocations)

handleMoveToEither :: State -> PersonName -> LocationName -> LocationName -> State
handleMoveToEither state personName locationName locationName2 =
  let
    newLocation = OneOfTwo ((Position locationName), (Position locationName2))
  in
    updateStateByMovement state personName newLocation


handleTake :: State -> PersonName -> ItemName -> State
handleTake state personName itemName =
  let
    existingPerson = getPersonWithName personName (people state)
    existingItem = find (((==) itemName) . iName) (items state)
  in
    case (existingPerson, existingItem) of
      ((Just person), (Just item)) ->
        state { items = (delete item (items state)) ++ [(Item itemName ((last . locations) person) existingPerson)] }
      ((Just person), Nothing) ->
        state { items = (items state) ++ [(Item itemName ((last . locations) person) existingPerson)] }
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
    isRightPerson = (((==) personName) . name)
    existingPerson = getPersonWithName personName (people state)
    existingItem = find (((==) itemName) . iName) (items state)
    updateItem item =
      let
        needsUpdate = fromMaybe False (fmap isRightPerson (iPerson item))
      in
        if needsUpdate then
          item
          { iPerson = Nothing
          }
        else item
  in
    case (existingPerson, existingItem) of
      ((Just person), (Just item)) ->
        state
        { items = map updateItem (items state)
        }
      _ ->
        state

update :: Command -> State -> State
update command state = case command of
  Move (EPerson (Ident personName)) (ELocation (Ident locationName)) ->
    handleMove state personName locationName

  Took (EPerson (Ident personName)) (EItem (Ident itemName)) ->
    handleTake state personName itemName

  Dropped (EPerson (Ident personName)) (EItem (Ident itemName)) ->
    handleDrop state personName itemName

  Handed (EPerson (Ident personName)) (EItem (Ident itemName)) (EPerson (Ident personName2)) ->
    handleGive state personName itemName personName2

  EitherIn (EPerson (Ident personName)) (ELocation (Ident locationName)) (ELocation (Ident locationName2)) ->
    handleMoveToEither state personName locationName locationName2

  NoLongerIn (EPerson (Ident personName)) (ELocation (Ident locationName)) ->
    handleLeave state personName locationName


counter state = do
  line <- getLine
  if null line then return () else do
    let Ok e = pCommand (myLexer line) in case e of
      WhereIs (EItem (Ident itemName)) -> do
        putStrLn $ whereIs state itemName
        counter state

      IsIn (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
        putStrLn $ isPersonIn state personName locationName
        counter state

      HowMany (EPerson (Ident personName)) -> do
        putStrLn $ howManyObjects state personName
        counter state

      WhereWasBefore (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
        putStrLn $ whereWasBefore state personName locationName
        counter state

      WhereWasAfter (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
        putStrLn $ whereWasAfter state personName locationName
        counter state
      _ -> do
        counter $ update e state

main = do
  counter (State [] [])
