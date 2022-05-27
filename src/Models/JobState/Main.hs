module Models.JobState.Main
  ( JobState(..)
  , toBuild
  , toText
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Models.Build.Main as Build
import qualified Models.BuildState.Main as BuildState

data JobState =
    Queued
  | Assigned
  | Scheduled Build.Build
  deriving (Eq, Show)

toBuild :: JobState -> Maybe Build.Build
toBuild state =
  case state of
    Scheduled bx -> return bx
    _            -> Nothing

toText :: JobState -> Text
toText sx =
  case sx of
    Queued       ->
      "queued"
    Assigned     ->
      "assigned"
    Scheduled bx ->
      BuildState.toText bx.state
