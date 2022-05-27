module Modules.Docker.Models.ContainerCheckOptions.Main
  ( ContainerCheckOptions(..)
  ) where

import RIO
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID
import qualified Modules.Docker.Models.ContainerCreateResponse.Main as ContainerCreateResponse

data ContainerCheckOptions = ContainerCheckOptions
  { containerID :: ContainerID.ContainerID
  }
  deriving (Eq, Show)

