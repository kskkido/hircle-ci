module Services.Build.Modules.Docker.Main
  ( Module(..)
  , simple
  ) where

import RIO
import qualified System.IO
import qualified Modules.Docker.Main as Docker
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID
import qualified Modules.Docker.Models.ContainerStatus.Main as ContainerStatus
import qualified Modules.Docker.Models.ContainerCheckOptions.Main as ContainerCheckOptions
import qualified Modules.Docker.Models.ContainerCheckResponse.Main as ContainerCheckResponse
import qualified Modules.Docker.Models.ContainerCreateOptions.Main as ContainerCreateOptions
import qualified Modules.Docker.Models.ContainerCreateResponse.Main as ContainerCreateResponse
import qualified Modules.Docker.Models.ContainerStartOptions.Main as ContainerStartOptions
import qualified Modules.Docker.Models.ImageCreateOptions.Main as ImageCreateOptions
import qualified Modules.Docker.Models.LogGetOptions.Main as LogGetOptions
import qualified Modules.Docker.Models.LogGetResponse.Main as LogGetResponse
import qualified Modules.Docker.Models.VolumeCreateOptions.Main as VolumeCreateOptions
import qualified Modules.Docker.Models.VolumeCreateResponse.Main as VolumeCreateResponse

data Module = Module
  { createContainer :: ContainerCreateOptions.ContainerCreateOptions -> System.IO.IO ContainerCreateResponse.ContainerCreateResponse
  , startContainer  :: ContainerStartOptions.ContainerStartOptions -> System.IO.IO ()
  , checkContainer  :: ContainerCheckOptions.ContainerCheckOptions -> System.IO.IO ContainerCheckResponse.ContainerCheckResponse
  , createVolume    :: VolumeCreateOptions.VolumeCreateOptions -> System.IO.IO VolumeCreateResponse.VolumeCreateResponse
  , createImage     :: ImageCreateOptions.ImageCreateOptions -> System.IO.IO ()
  }

simple :: Module
simple = Module
  { createContainer = Docker.createContainer
  , startContainer  = Docker.startContainer
  , checkContainer  = Docker.checkContainer
  , createVolume    = Docker.createVolume
  , createImage     = Docker.createImage
  }
