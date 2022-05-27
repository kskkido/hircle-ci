module Services.Log.Modules.Docker.Main
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
import qualified Modules.Docker.Models.LogGetOptions.Main as LogGetOptions
import qualified Modules.Docker.Models.LogGetResponse.Main as LogGetResponse
import qualified Modules.Docker.Models.VolumeCreateOptions.Main as VolumeCreateOptions
import qualified Modules.Docker.Models.VolumeCreateResponse.Main as VolumeCreateResponse

data Module = Module
  { getLogs         :: LogGetOptions.LogGetOptions -> System.IO.IO LogGetResponse.LogGetResponse
  }

simple :: Module
simple = Module
  { getLogs         = Docker.getLogs
  }
