module Location where

import Utils

type LocationName = String

newtype Position = Position String deriving (Eq, Show)

data Location
  = Known Position
  | OneOfTwo (Position, Position)
  | NoLonger Position
  deriving (Eq, Show)

isCancelling [(Known loc), (NoLonger loc2)] = loc == loc2
isCancelling _ = False

filterOutCancelling :: [Location] -> [Location]
filterOutCancelling locations =
  reverse $
  concat $
  (not . isCancelling . reverse) `filter`
  (chunk' (reverse locations))