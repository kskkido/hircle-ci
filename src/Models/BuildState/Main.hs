module Models.BuildState.Main
  ( BuildState(..)
  , toBuildResult
  , toBuildRunningState
  , toText
  ) where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Models.BuildResult.Main as BuildResult
import qualified Models.BuildRunningState.Main as BuildRunningState

data BuildState =
    Ready
  | Running BuildRunningState.BuildRunningState
  | Complete BuildResult.BuildResult
  deriving (Eq, Show, Generic, Serialise.Serialise)

toBuildResult :: BuildState -> Maybe BuildResult.BuildResult
toBuildResult sx =
  case sx of
    Complete rx -> return rx
    _           -> Nothing

toBuildRunningState :: BuildState -> Maybe BuildRunningState.BuildRunningState
toBuildRunningState sx =
  case sx of
    Running rx -> return rx
    _          -> Nothing


toText :: BuildState -> Text
toText sx =
  case sx of
    Ready       ->
      "ready"
    Running  _  ->
      "running"
    Complete br ->
      BuildResult.toText br
