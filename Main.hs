module Main where

import LexAI
import ParAI
import AbsAI
import ErrM

import State

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


loop state = do
  line <- getLine
  if null line then return () else do
    let Ok e = pCommand (myLexer line) in case e of
      WhereIs (EItem (Ident itemName)) -> do
        putStrLn $ whereIsItem state itemName
        loop state

      IsIn (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
        putStrLn $ isPersonIn state personName locationName
        loop state

      HowMany (EPerson (Ident personName)) -> do
        putStrLn $ howManyObjects state personName
        loop state

      WhereWasBefore (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
        putStrLn $ whereWasBefore state personName locationName
        loop state

      WhereWasAfter (EPerson (Ident personName)) (ELocation (Ident locationName)) -> do
        putStrLn $ whereWasAfter state personName locationName
        loop state
      _ -> do
        loop $ update e state

main = do
  loop (State [] [])
