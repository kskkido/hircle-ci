module Models.Step.Main
  ( Step(..)
  , fromCommit
  , toContainerCreateOptions
  ) where

import RIO
import qualified RIO.Text
import qualified RIO.NonEmpty
import qualified Data.Aeson as Aeson
import qualified Codec.Serialise as Serialise
import qualified Modules.Docker.Models.Image.Main as Docker.Image
import qualified Modules.Docker.Models.Volume.Main as Docker.Volume
import qualified Modules.Docker.Models.ContainerCreateOptions.Main as Docker.ContainerCreateOptions
import qualified Modules.Github.Models.Commit.Main as Commit
import qualified Models.StepName.Main as StepName

data Step = Step
  { commands :: NonEmpty Text
  , name     :: StepName.StepName
  , image    :: Docker.Image.Image
  } deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

fromCommit :: Commit.Commit -> Step
fromCommit cmx = Step
  { name     = StepName.StepName "clone"
  , image    = Docker.Image.Image "alpine/git" "v2.26.2"
  , commands = (
        "git clone -q https://github.com/" <> cmx.repository <> " ."
      :|
        [ "git checkout -qf " <> cmx.sha
        ]
    )
  }

toContainerCreateOptions :: Maybe Docker.Volume.Volume -> Step -> Docker.ContainerCreateOptions.ContainerCreateOptions
toContainerCreateOptions mvx step = Docker.ContainerCreateOptions.ContainerCreateOptions
  { image  = step.image
  , script = RIO.Text.unlines $ ["set -ex"] <> RIO.NonEmpty.toList step.commands
  , volume = mvx
  }

