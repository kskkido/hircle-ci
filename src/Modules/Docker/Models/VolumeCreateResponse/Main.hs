module Modules.Docker.Models.VolumeCreateResponse.Main
  ( VolumeCreateResponse(..)
  , fromJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Modules.Docker.Models.Volume.Main as Volume

data VolumeCreateResponse = VolumeCreateResponse
  { name :: Volume.Volume
  }
  deriving (Eq, Show)

fromJSON :: Aeson.Value -> Aeson.Types.Parser VolumeCreateResponse
fromJSON = Aeson.withObject "create-volume" $ \o ->
  o Aeson..: "Name" <&> VolumeCreateResponse . Volume.Volume
