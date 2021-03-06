module Main where

import Control.Monad

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

  IsOf (ELocation (Ident locationName)) direction (ELocation (Ident locationName2)) ->
    storeDirection state locationName direction locationName2

loop state = do
  line <- getLine
  unless (null line) $
    let Ok e = pCommand (myLexer line) in
      case e of
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
        HowToGo (ELocation (Ident locationName)) (ELocation (Ident locationName2)) -> do
          putStrLn $ howToGo state locationName locationName2
          loop state
        _ -> do
        let newState = update e state
        loop newState

main = loop (State [] [] [])
