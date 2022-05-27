module Models.LogCollectionStatus.Main
  ( LogCollectionStatus(..)
  ) where

import qualified Data.Time.Clock.POSIX as Time
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID

data LogCollectionStatus =
    Ready
  | Complete
  | Running ContainerID.ContainerID Time.POSIXTime
