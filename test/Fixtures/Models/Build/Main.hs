module Fixtures.Models.Build.Main
  ( test1
  , test2
  , test3
  , test4
  , test5
  , test6
  ) where

import RIO
import qualified RIO.NonEmpty
import qualified System.IO
import qualified Data.Yaml
import qualified Modules.Docker.Models.Image.Main as Image
import qualified Modules.Docker.Models.Volume.Main as Volume
import qualified Models.Build.Main as Build
import qualified Models.BuildState.Main as BuildState
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName
import qualified Models.StepResult.Main as StepResult
import qualified Services.Build.Modules.Docker.Main as Docker

test1 :: Build.Build
test1 = Build.Build
  { pipeline       = Pipeline.Pipeline
    { steps =
      Step.Step
        { name     = StepName.StepName "First"
        , commands = pure "date"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        } :|
      [ Step.Step
        { name     = StepName.StepName "Second"
        , commands = pure "uname -r"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        }
      ]
    }
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = Nothing
  }

test2 :: Build.Build
test2 = Build.Build
  { pipeline       = Pipeline.Pipeline
    { steps =
      Step.Step
        { name     = StepName.StepName "First"
        , commands = pure "date"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        } :|
      [ Step.Step
        { name     = StepName.StepName "Second"
        , commands = pure "exit 1"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        }
      ]
    }
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = Nothing
  }

test3 :: Volume.Volume -> Build.Build
test3 vx = Build.Build
  { pipeline       = Pipeline.Pipeline
    { steps =
      Step.Step
        { name     = StepName.StepName "First"
        , commands = pure "echo hello > test"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        } :|
      [ Step.Step
        { name     = StepName.StepName "Second"
        , commands = pure "cat test"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        }
      ]
    }
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = return vx
  }

test4 :: Build.Build
test4 = Build.Build
  { pipeline       = Pipeline.Pipeline
    { steps =
      Step.Step
        { name     = StepName.StepName "First"
        , commands = "echo hello" :| ["sleep 2", "echo world"]
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        } :|
      [ Step.Step
        { name     = StepName.StepName "Second"
        , commands = pure "uname -s"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        }
      ]
    }
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = Nothing
  }

test5 :: Build.Build
test5 = Build.Build
  { pipeline       = Pipeline.Pipeline
    { steps =
      Step.Step
        { name     = StepName.StepName "First"
        , commands = pure "date"
        , image    = Image.Image
            { Image.name = "busybox"
            , Image.tag  = "latest"
            }
        } :|
      [ Step.Step
        { name     = StepName.StepName "Second"
        , commands = pure "uname -s"
        , image    = Image.Image
            { Image.name = "ubuntu"
            , Image.tag  = "latest"
            }
        }
      ]
    }
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = Nothing
  }

test6 :: Pipeline.Pipeline -> Build.Build
test6 pipeline = Build.Build
  { pipeline       = pipeline
  , state          = BuildState.Ready
  , completedSteps = mempty
  , volume         = Nothing
  }
