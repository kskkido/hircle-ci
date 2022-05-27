module Modules.Docker.Models.ContainerCheckResponse.Main
  ( ContainerCheckResponse(..)
  , fromJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID
import qualified Modules.Docker.Models.ContainerStatus.Main as ContainerStatus

data ContainerCheckResponse = ContainerCheckResponse
  { status :: ContainerStatus.ContainerStatus
  }
  deriving (Eq, Show)

fromJSON :: Aeson.Value -> Aeson.Types.Parser ContainerCheckResponse
fromJSON = Aeson.withObject "container-inspect" $ \o -> do
  o Aeson..: "State" >>= ContainerStatus.fromStateObject >>= return . ContainerCheckResponse

