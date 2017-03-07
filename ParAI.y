-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParAI where
import AbsAI
import LexAI
import ErrM

}

%name pCommand Command
%name pELocation ELocation
%name pEItem EItem
%name pEPerson EPerson
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '?' { PT _ (TS _ 1) }
  'How' { PT _ (TS _ 2) }
  'Is' { PT _ (TS _ 3) }
  'Where' { PT _ (TS _ 4) }
  'after' { PT _ (TS _ 5) }
  'before' { PT _ (TS _ 6) }
  'carrying' { PT _ (TS _ 7) }
  'discarded' { PT _ (TS _ 8) }
  'do' { PT _ (TS _ 9) }
  'dropped' { PT _ (TS _ 10) }
  'either' { PT _ (TS _ 11) }
  'from' { PT _ (TS _ 12) }
  'go' { PT _ (TS _ 13) }
  'got' { PT _ (TS _ 14) }
  'handed' { PT _ (TS _ 15) }
  'in' { PT _ (TS _ 16) }
  'is' { PT _ (TS _ 17) }
  'journeyed' { PT _ (TS _ 18) }
  'longer' { PT _ (TS _ 19) }
  'many' { PT _ (TS _ 20) }
  'moved' { PT _ (TS _ 21) }
  'no' { PT _ (TS _ 22) }
  'objects' { PT _ (TS _ 23) }
  'or' { PT _ (TS _ 24) }
  'picked' { PT _ (TS _ 25) }
  'the' { PT _ (TS _ 26) }
  'to' { PT _ (TS _ 27) }
  'took' { PT _ (TS _ 28) }
  'travelled' { PT _ (TS _ 29) }
  'up' { PT _ (TS _ 30) }
  'was' { PT _ (TS _ 31) }
  'went' { PT _ (TS _ 32) }
  'you' { PT _ (TS _ 33) }

L_ident  { PT _ (TV $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Command :: { Command }
Command : 'How' 'many' 'objects' 'is' EPerson 'carrying' '?' { AbsAI.HowMany $5 }
        | 'Where' 'is' EItem '?' { AbsAI.WhereIs $3 }
        | 'Where' 'was' EPerson 'before' ELocation '?' { AbsAI.WhereWasBefore $3 $5 }
        | 'Where' 'was' EPerson 'after' ELocation '?' { AbsAI.WhereWasAfter $3 $5 }
        | 'How' 'do' 'you' 'go' ELocation ELocation '?' { AbsAI.HowToGo $5 $6 }
        | EPerson 'is' 'either' ELocation ELocation { AbsAI.EitherIn $1 $4 $5 }
        | EPerson 'is' 'no' 'longer' ELocation { AbsAI.NoLongerIn $1 $5 }
        | 'Is' EPerson ELocation '?' { AbsAI.IsIn $2 $3 }
        | EPerson 'dropped' EItem { AbsAI.Dropped $1 $3 }
        | EPerson 'discarded' EItem { AbsAI.Dropped $1 $3 }
        | EPerson 'handed' EItem 'to' EPerson { AbsAI.Handed $1 $3 $5 }
        | EPerson 'moved' ELocation { AbsAI.Move $1 $3 }
        | EPerson 'journeyed' ELocation { AbsAI.Move $1 $3 }
        | EPerson 'went' ELocation { AbsAI.Move $1 $3 }
        | EPerson 'travelled' ELocation { AbsAI.Move $1 $3 }
        | EPerson 'is' ELocation { AbsAI.Move $1 $3 }
        | EPerson 'took' EItem { AbsAI.Took $1 $3 }
        | EPerson 'got' EItem { AbsAI.Took $1 $3 }
        | EPerson 'picked' 'up' EItem { AbsAI.Took $1 $4 }
ELocation :: { ELocation }
ELocation : 'or' 'the' Ident { AbsAI.ELocation $3 }
          | 'to' 'the' Ident { AbsAI.ELocation $3 }
          | 'in' 'the' Ident { AbsAI.ELocation $3 }
          | 'from' 'the' Ident { AbsAI.ELocation $3 }
          | 'the' Ident { AbsAI.ELocation $2 }
EItem :: { EItem }
EItem : 'the' Ident { AbsAI.EItem $2 }
EPerson :: { EPerson }
EPerson : Ident { AbsAI.EPerson $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}
