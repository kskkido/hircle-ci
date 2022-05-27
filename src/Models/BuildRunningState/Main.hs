module Models.BuildRunningState.Main
  ( BuildRunningState(..)
  , fromStep
  ) where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Models.Step.Main as Step
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID

data BuildRunningState = BuildRunningState
  { step        :: StepName.StepName
  , containerID :: ContainerID.ContainerID
  } deriving (Eq, Show, Generic, Serialise.Serialise)

fromStep :: Step.Step -> ContainerID.ContainerID -> BuildRunningState
fromStep sx cx = BuildRunningState
  { step        = Step.name sx
  , containerID = cx
  }
