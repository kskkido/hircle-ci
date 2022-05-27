module Fixtures.Models.Pipeline.Main
  ( test1
  ) where

import RIO
import qualified RIO.NonEmpty
import qualified Modules.Docker.Models.Image.Main as Image
import qualified Modules.Docker.Models.Volume.Main as Volume
import qualified Models.Build.Main as Build
import qualified Models.BuildState.Main as BuildState
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName
import qualified Models.StepResult.Main as StepResult

test1 :: Pipeline.Pipeline
test1 = Pipeline.Pipeline
  { Pipeline.steps = Step.Step
      { Step.name     = StepName.StepName "Agent test"
      , Step.commands = "echo hello" :| ["echo from"]
      , Step.image    = Image.Image
          { Image.name = "busybox"
          , Image.tag  = "latest"
          }
      } :| []
  }
