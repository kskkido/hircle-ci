module Models.BuildStepDTO.Main
  ( BuildStepDTO(..)
  , fromStep
  , fromBuildStep
  , toJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Models.Build.Main as Build
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName

data BuildStepDTO = BuildStepDTO
  { name  :: Text
  , state :: Text
  }

fromStep :: Step.Step -> BuildStepDTO
fromStep sx = BuildStepDTO
  { name  = StepName.toText sx.name
  , state = "ready"
  }

fromBuildStep :: Build.Build -> Step.Step -> BuildStepDTO
fromBuildStep bx sx = BuildStepDTO
  { name  = StepName.toText sx.name
  , state = Build.toBuildStepStateText sx bx
  }

toJSON :: BuildStepDTO -> Aeson.Value
toJSON dto = Aeson.object $
  [ ("name",  Aeson.toJSON $ dto.name)
  , ("state", Aeson.toJSON $ dto.state)
  ]
