module Models.Build.Main
  ( Build(..)
  , fromPipeline
  , toBuildStepStateText
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified Data.Aeson as Aeson
import qualified System.IO
import qualified Codec.Serialise as Serialise
import qualified Modules.Docker.Models.ContainerExitCode.Main as Docker.ContainerExitCode
import qualified Modules.Docker.Models.Volume.Main as Docker.Volume
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.BuildResult.Main as BuildResult
import qualified Models.BuildState.Main as BuildState
import qualified Models.BuildRunningState.Main as BuildRunningState
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName
import qualified Models.StepResult.Main as StepResult

data Build = Build
  { pipeline       :: Pipeline.Pipeline
  , state          :: BuildState.BuildState
  , completedSteps :: Map StepName.StepName StepResult.StepResult
  , volume         :: Maybe Docker.Volume.Volume
  } deriving (Eq, Show, Generic, Serialise.Serialise)

fromPipeline :: Pipeline.Pipeline -> Build
fromPipeline px = Build
  { pipeline       = px
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = Nothing
  }

toBuildStepStateText :: Step.Step -> Build -> Text
toBuildStepStateText sx bx = maybe "" id $ asum
  [ do
      brx <- BuildState.toBuildRunningState bx.state
      guard (brx.step == sx.name)
      return $ "running"
  , do
      srx <- Map.lookup sx.name bx.completedSteps
      return $ StepResult.toText srx
  , case bx.state of
      BuildState.Complete _ -> return "skipped"
      _                     -> return "ready"
  ]

