module Models.StepResult.Main
  ( StepResult(..)
  , fromContainerExitCode
  , toText
  ) where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Modules.Docker.Models.ContainerExitCode.Main as Docker.ContainerExitCode

data StepResult =
    Success
  | Failure Docker.ContainerExitCode.ContainerExitCode
  deriving (Eq, Show, Generic, Serialise.Serialise)

fromContainerExitCode :: Docker.ContainerExitCode.ContainerExitCode -> StepResult
fromContainerExitCode x = maybe Success Failure $ do
  guard $ Docker.ContainerExitCode.toInt x /= 0
  return x

toText :: StepResult -> Text
toText sx =
  case sx of
    Success   ->
      "success"
    Failure _ ->
      "failure"
