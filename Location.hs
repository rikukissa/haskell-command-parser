module Location where

import Utils

type LocationName = String

newtype Position = Position String deriving (Eq, Show)

data Location
  = Known Position
  | OneOfTwo (Position, Position)
  | NoLonger Position
  | MaybeIn Position

  deriving (Eq, Show)
