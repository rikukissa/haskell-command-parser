module Main where

import Data.List
import Data.Maybe

import LexCalc
import ParCalc
import AbsCalc
import ErrM

type PersonName = String
type ItemName = String
type LocationName = String

data Person = Person
  { name :: PersonName
  , location :: LocationName
  } deriving (Show, Eq)

data Item = Item
  { iName :: ItemName
  , iLocation :: LocationName
  , iPerson :: Maybe Person
  } deriving (Show, Eq)

data State = State
  { people :: [Person]
  , items :: [Item]
  } deriving (Show)

isPersonIn :: State -> PersonName -> LocationName -> String
isPersonIn state personName locationName =
  let
    boolToAnswer b = if b then "yes" else "no"
    existingPerson = find (((==) personName) . name) (people state)
    isThere = fmap (boolToAnswer . ((==) locationName) . location) existingPerson
  in
    fromMaybe "maybe" isThere

handleMove :: State -> PersonName -> LocationName -> State
handleMove state personName locationName =
  let
    updatedPerson = (Person personName locationName)
    existingPerson = find (((==) personName) . name) (people state)
  in
    case existingPerson of
      Just person ->
        state { people = (delete person (people state)) ++ [updatedPerson] }
      Nothing ->
        state { people = (people state) ++ [updatedPerson] }

handleTake :: State -> PersonName -> ItemName -> State
handleTake state personName itemName =
  let
    existingPerson = find (((==) personName) . name) (people state)
    existingItem = find (((==) itemName) . iName) (items state)
  in
    case (existingPerson, existingItem) of
      ((Just person), (Just item)) ->
        state { items = (delete item (items state)) ++ [(Item itemName (location person) existingPerson)] }
      ((Just person), Nothing) ->
        state { items = (items state) ++ [(Item itemName (location person) existingPerson)] }
      _ ->
        state

update :: Exp -> State -> State
update command state = case command of
  EMove (EString (Ident personName)) (ELocation (EString (Ident locationName))) ->
    handleMove state personName locationName

  ETook (EString (Ident personName)) (EItem (EString (Ident itemName))) ->
    handleTake state personName itemName

  EDrop person item ->
    state

  EGive person item person2 ->
    state

counter state = do
  line <- getLine
  if null line then return () else do
    let Ok e = pExp (myLexer line) in case e of
      EIs (EString (Ident personName)) (ELocation (EString (Ident locationName))) -> do
        putStrLn $ isPersonIn state personName locationName
        counter state
      _ ->
        counter $ update e state

main = do
  counter (State [] [])
