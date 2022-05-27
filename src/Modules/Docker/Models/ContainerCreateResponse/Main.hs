module Modules.Docker.Models.ContainerCreateResponse.Main
  ( ContainerCreateResponse(..)
  , fromJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID

data ContainerCreateResponse = ContainerCreateResponse
  { containerID :: ContainerID.ContainerID
  }
  deriving (Eq, Show)

fromJSON :: Aeson.Value -> Aeson.Types.Parser ContainerCreateResponse
fromJSON = Aeson.withObject "create-container" $ \o ->
  o Aeson..: "Id" <&> ContainerCreateResponse . ContainerID.ContainerID
