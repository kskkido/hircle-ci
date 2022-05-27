module Modules.Docker.Models.ContainerStartOptions.Main
  ( ContainerStartOptions(..)
  , fromContainerCreateResponse
  ) where

import RIO
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID
import qualified Modules.Docker.Models.ContainerCreateResponse.Main as ContainerCreateResponse

data ContainerStartOptions = ContainerStartOptions
  { containerID :: ContainerID.ContainerID
  }
  deriving (Eq, Show)

fromContainerCreateResponse :: ContainerCreateResponse.ContainerCreateResponse -> ContainerStartOptions
fromContainerCreateResponse rx = ContainerStartOptions rx.containerID
