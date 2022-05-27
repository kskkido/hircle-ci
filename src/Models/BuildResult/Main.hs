module Models.BuildResult.Main
  ( BuildResult(..)
  , toText
  ) where

import RIO
import qualified Codec.Serialise as Serialise

data BuildResult =
    Success
  | Failure
  | Unknown Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

toText :: BuildResult -> Text
toText br =
  case br of
    Success   ->
      "success"
    Failure   ->
      "failure"
    Unknown t ->
      "unknown"
