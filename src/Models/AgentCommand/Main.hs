module Models.AgentCommand.Main
  ( AgentCommand(..)
  ) where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Models.Pipeline.Main as Pipeline

data AgentCommand =
    StartBuild BuildNumber.BuildNumber Pipeline.Pipeline
  deriving (Eq, Show, Generic, Serialise.Serialise)
