module Modules.Docker.Models.ContainerStartResponse.Main
  ( ContainerStartResponse(..)
  , fromJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID

data ContainerStartResponse = ContainerStartResponse
  { containerID :: ContainerID.ContainerID
  }
  deriving (Eq, Show)

fromJSON :: Aeson.Value -> Aeson.Types.Parser ContainerStartResponse
fromJSON = Aeson.withObject "create-container" $ \o ->
  o Aeson..: "id" <&> ContainerStartResponse . ContainerID.ContainerID
