module Models.Job.Main
  ( Job(..)
  , scheduleBuild
  , toJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Modules.Github.Models.Commit.Main as Commit
import qualified Models.Step.Main as Step
import qualified Models.Build.Main as Build
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Models.BuildStepDTO.Main as BuildStepDTO
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.JobState.Main as JobState

data Job = Job
  { state    :: JobState.JobState
  , pipeline :: Pipeline.Pipeline
  , commit   :: Commit.Commit
  }

scheduleBuild :: Build.Build -> Job -> Job
scheduleBuild bx jx = jx
  { state = JobState.Scheduled bx
  }

toJSON :: BuildNumber.BuildNumber -> Job -> Aeson.Value
toJSON bnx jx = Aeson.object $
  [ ("number", Aeson.toJSON $ BuildNumber.toInt bnx)
  , ("state",  Aeson.toJSON $ JobState.toText jx.state)
  , ("commit", Aeson.toJSON $ jx.commit)
  , ("steps",  Aeson.toJSON $ jx.pipeline.steps <&> (
      BuildStepDTO.toJSON <$>
      maybe BuildStepDTO.fromStep BuildStepDTO.fromBuildStep mbx
    ))
  ]
  where mbx = JobState.toBuild jx.state
